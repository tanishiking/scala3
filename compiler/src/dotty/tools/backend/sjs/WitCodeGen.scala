package dotty.tools.backend.sjs

import scala.language.unsafeNulls

import scala.collection.mutable

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import Contexts.*
import Decorators.*
import Flags.*
import Names.*
import Phases.*
import Types.*
import Symbols.*
import StdNames.*

import dotty.tools.dotc.util.SourcePosition

import dotty.tools.sjs.ir
import dotty.tools.sjs.ir.{ClassKind, Position, Trees => js, Types => jstpe, WellKnownNames => jswkn}
import dotty.tools.sjs.ir.{WasmInterfaceTypes => wit}
import dotty.tools.sjs.ir.Names.{ClassName, FieldName, LocalName, MethodName, SimpleFieldName, SimpleMethodName}
import dotty.tools.sjs.ir.OriginalName

import dotty.tools.dotc.transform.sjs.JSSymUtils.*

import JSEncoding.*

/** Code generation for Wasm Component Model (WIT) interop.
 *
 *  This class encapsulates all WIT-specific code generation logic,
 *  including detection of WIT annotations, type conversion to WIT types,
 *  and generation of WIT IR nodes.
 */
class WitCodeGen(jsCodeGen: JSCodeGen)(using Context) {
  import tpd.*
  import jsCodeGen.positionConversions.span2irPos

  private val jsdefn = JSDefinitions.jsdefn

  // State tracking for WIT exports and function types
  private val witExports = mutable.Map.empty[Symbol, WitExportInfo]
  private val witFunctionTypes = mutable.Map.empty[Symbol, WitFunctionType]
  private val witVariantValueTypes = mutable.Map.empty[Symbol, Type]

  // Detection methods

  /** Is this type a WIT resource type? */
  def isWasmWitResourceType(tpe: Type): Boolean =
    isWasmWitResourceType(tpe.typeSymbol)

  /** Is this symbol a WIT resource type? */
  def isWasmWitResourceType(sym: Symbol): Boolean =
    sym.hasAnnotation(jsdefn.WitResourceImportAnnot)

  /** Is this a WIT record class? */
  def isWasmWitRecordClass(sym: Symbol): Boolean =
    sym.hasAnnotation(jsdefn.WitRecordAnnot) && sym.is(Final)

  /** Is this a WIT resource static method? */
  def isWasmWitResourceStaticMethod(sym: Symbol): Boolean =
    sym.isWitResourceStaticMethod

  /** Is this a WIT resource constructor? */
  def isWasmWitResourceConstructor(sym: Symbol): Boolean =
    sym.isWitResourceConstructor

  /** Is this a WIT component tuple class? */
  def isWasmComponentTupleClass(sym: Symbol): Boolean =
    sym.isWitComponentTupleClass

  /** Is this a WIT flags type? */
  def isWasmWitFlags(sym: Symbol): Boolean =
    sym.isWitFlags

  /** Check if this tree is a call to a WIT native member (method annotated with @WitImport or similar). */
  def isWitNativeMemberCall(tree: Tree): Boolean = tree match {
    case Apply(fun, _) =>
      val sym = fun.symbol
      sym.hasAnnotation(jsdefn.WitImportAnnot) ||
      sym.hasAnnotation(jsdefn.WitResourceMethodAnnot) ||
      sym.hasAnnotation(jsdefn.WitResourceDropAnnot) ||
      sym.hasAnnotation(jsdefn.WitResourceStaticMethodAnnot) ||
      sym.hasAnnotation(jsdefn.WitResourceConstructorAnnot)
    case _ => false
  }

  /** Check if this symbol has any WIT native annotation. */
  def hasWitNativeAnnotation(sym: Symbol): Boolean =
    sym.hasAnnotation(jsdefn.WitImportAnnot) ||
    sym.hasAnnotation(jsdefn.WitResourceMethodAnnot) ||
    sym.hasAnnotation(jsdefn.WitResourceDropAnnot) ||
    sym.hasAnnotation(jsdefn.WitResourceStaticMethodAnnot) ||
    sym.hasAnnotation(jsdefn.WitResourceConstructorAnnot)

  // Code generation methods

  /** Generate a WitFunctionApply node for a call to a WIT native member. */
  def genWitNativeMemberCall(method: Symbol, tree: Apply, receiver: Option[Tree], isStat: Boolean)(
      using codeGen: JSCodeGen, localNames: LocalNameGenerator): js.Tree = {
    implicit val pos: Position = span2irPos(tree.span)

    val methodIdent = encodeMethodSym(method)
    val className = encodeClassName(method.owner)

    js.WitFunctionApply(
      receiver.map(codeGen.genExpr(_)),
      className,
      methodIdent,
      tree.args.map(codeGen.genExpr(_))
    )(toIRType(tree.tpe))
  }

  /** Generate a ClassDef for a WIT resource class (trait annotated with @WitResourceImport). */
  def genWasmComponentResourceClassData(td: TypeDef)(using localNames: LocalNameGenerator): js.ClassDef = {
    val sym = td.symbol
    implicit val pos: Position = span2irPos(sym.span)

    val classIdent = encodeClassNameIdent(sym)
    val kind = ClassKind.NativeWasmComponentResourceClass

    val annot = sym.getAnnotation(jsdefn.WitResourceImportAnnot).get
    val moduleName = annot.argumentConstantString(0).get
    val resourceName = annot.argumentConstantString(1).get

    val flags = js.MemberFlags.empty.withNamespace(js.MemberNamespace.Public)
    val witNativeMembersBuilder = List.newBuilder[js.WitNativeMemberDef]

    td.rhs match {
      case impl: Template =>
        for (stat <- impl.body) {
          stat match {
            case dd: DefDef if dd.symbol.hasAnnotation(jsdefn.WitResourceMethodAnnot) =>
              for {
                annot <- dd.symbol.getAnnotation(jsdefn.WitResourceMethodAnnot)
                functionName <- annot.argumentConstantString(0)
              } {
                witNativeMembersBuilder +=
                  genWitNativeMemberDef(flags, dd, moduleName,
                    js.WitFunctionName.ResourceMethod(functionName, resourceName))
              }

            case dd: DefDef if dd.symbol.hasAnnotation(jsdefn.WitResourceDropAnnot) =>
              for {
                _ <- dd.symbol.getAnnotation(jsdefn.WitResourceDropAnnot)
              } {
                witNativeMembersBuilder +=
                  genWitNativeMemberDef(flags, dd, moduleName,
                    js.WitFunctionName.ResourceDrop(resourceName))
              }

            case _ =>
          }
        }
      case _ =>
    }

    js.ClassDef(
      classIdent,
      originalNameOfClass(sym),
      kind,
      jsClassCaptures = None,
      superClass = None,
      interfaces = Nil,
      jsSuperClass = None,
      jsNativeLoadSpec = None,
      fields = Nil,
      methods = Nil,
      jsConstructor = None,
      jsMethodProps = Nil,
      jsNativeMembers = Nil,
      witNativeMembers = witNativeMembersBuilder.result(),
      topLevelExportDefs = Nil
    )(js.OptimizerHints.empty)
  }

  /** Generate a WitNativeMemberDef for a WIT imported function. */
  def genWitNativeMemberDef(flags: js.MemberFlags, dd: DefDef, moduleName: String,
      name: js.WitFunctionName)(using localNames: LocalNameGenerator): js.WitNativeMemberDef = {
    implicit val pos: Position = span2irPos(dd.span)
    val sym = dd.symbol

    val funcType = witFunctionTypeOf(sym)
    val baseParams = funcType.params.map(toWIT(_))

    val params = name match {
      case _: js.WitFunctionName.Function |
           _: js.WitFunctionName.ResourceConstructor |
           _: js.WitFunctionName.ResourceStaticMethod =>
        baseParams
      case _: js.WitFunctionName.ResourceMethod |
           _: js.WitFunctionName.ResourceDrop =>
        wit.ResourceType(encodeClassName(sym.owner)) +: baseParams
    }

    val witFuncType = wit.FuncType(
      params,
      toResultWIT(funcType.resultType)
    )

    js.WitNativeMemberDef(flags, moduleName, name, encodeMethodSym(sym), witFuncType)
  }

  /** Generate a WitNativeMemberDef for a WIT resource static method. */
  def genWitResourceStaticMethodDef(dd: DefDef)(using localNames: LocalNameGenerator): Option[js.WitNativeMemberDef] = {
    implicit val pos: Position = span2irPos(dd.span)
    val sym = dd.symbol

    val flags = js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic)
    val funcType = witFunctionTypeOf(sym)

    for {
      methodAnnot <- sym.getAnnotation(jsdefn.WitResourceStaticMethodAnnot)
      resourceAnnot <- sym.owner.companionClass.getAnnotation(jsdefn.WitResourceImportAnnot)
      methodName <- methodAnnot.argumentConstantString(0)
      moduleName <- resourceAnnot.argumentConstantString(0)
      resourceName <- resourceAnnot.argumentConstantString(1)
    } yield {
      val name = js.WitFunctionName.ResourceStaticMethod(
        func = methodName, resource = resourceName)
      val params = funcType.params.map(p => toWIT(p))
      val ft = wit.FuncType(params, toResultWIT(funcType.resultType))
      js.WitNativeMemberDef(flags, moduleName, name, encodeMethodSym(sym), ft)
    }
  }

  /** Generate a WitNativeMemberDef for a WIT resource constructor. */
  def genWitResourceConstructor(dd: DefDef)(using localNames: LocalNameGenerator): Option[js.WitNativeMemberDef] = {
    implicit val pos: Position = span2irPos(dd.span)
    val sym = dd.symbol

    val flags = js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic)
    val funcType = witFunctionTypeOf(sym)

    for {
      _ <- sym.getAnnotation(jsdefn.WitResourceConstructorAnnot)
      resourceAnnot <- sym.owner.companionClass.getAnnotation(jsdefn.WitResourceImportAnnot)
      moduleName <- resourceAnnot.argumentConstantString(0)
      resourceName <- resourceAnnot.argumentConstantString(1)
    } yield {
      val name = js.WitFunctionName.ResourceConstructor(resourceName)
      val params = funcType.params.map(p => toWIT(p))
      val ft = wit.FuncType(params, toResultWIT(funcType.resultType))
      js.WitNativeMemberDef(flags, moduleName, name, encodeMethodSym(sym), ft)
    }
  }

  /** Generate a WitExportDef for a method annotated with @WitExport. */
  def genWitExportDef(info: WitExportInfo, methodDef: js.MethodDef)(
      using localNames: LocalNameGenerator): js.WitExportDef = {
    val signature = wit.FuncType(
      info.signature.params.map(toWIT(_)),
      toResultWIT(info.signature.resultType)
    )
    js.WitExportDef(
      info.moduleName,
      js.WitFunctionName.Function(info.name),
      methodDef,
      signature
    )(using methodDef.pos)
  }

  // Type conversion methods

  /** Convert a Scala type to a WIT ValType.
   *
   *  Uses pre-erasure phase context because the types may come from
   *  `witFunctionTypeOf` / `witVariantValueTypeOf` which capture types
   *  at `atPhase(erasurePhase)`. Type aliases (e.g., `Predef.String`)
   *  can only be resolved via `widenDealias` in that phase context.
   *
   *  This method uses symbol-based checks instead of `toIRType` because
   *  `toIRType` (in JSEncoding) does not handle pre-erasure `AppliedType`.
   */
  def toWIT(tpe: Type): wit.ValType = atPhase(erasurePhase) {
    val widened = tpe.widenDealias
    val tsym = widened.typeSymbol

    // First check for unsigned types
    unsigned2WIT.get(tsym).orElse {
      // Check for array types
      toWITMaybeArray(widened)
    }.orElse {
      // Check for primitive types and String via symbol
      primitiveSymbolWIT.get(tsym)
    }.getOrElse {
      if (isWasmComponentTupleClass(tsym)) {
        val args = widened match {
          case AppliedType(_, args) => args
          case _ => Nil
        }
        wit.TupleType(args.map(toWIT(_)))
      } else if (tsym.hasAnnotation(jsdefn.WitFlagsAnnot)) {
        // Read numFlags from annotation parameter
        val numFlags = tsym.getAnnotation(jsdefn.WitFlagsAnnot)
          .flatMap(_.argumentConstant(0).map(_.intValue))
          .getOrElse {
            throw new AssertionError(s"@WitFlags on $tsym missing numFlags parameter")
          }
        val className = encodeClassNamePostFlatten(tsym)
        wit.FlagsType(className, numFlags)
      } else if (isWasmWitRecordClass(tsym)) {
        val className = encodeClassNamePostFlatten(tsym)
        // At the erasure phase, val fields are represented as accessor methods
        // (they have both ParamAccessor and Method flags). Use the primary
        // constructor parameters to get the field types with full generic info.
        val primaryCtor = tsym.primaryConstructor
        val ctorParams = if (primaryCtor.exists) primaryCtor.info.firstParamTypes else Nil
        val accessors = tsym.info.decls.toList.filter(f =>
          f.is(ParamAccessor) && f.is(Accessor) && f.is(StableRealizable))
        val fields: List[wit.FieldType] = accessors.zip(ctorParams).map { case (f, paramTpe) =>
          implicit val pos: Position = span2irPos(f.span)
          // Construct FieldName manually since encodeFieldSym requires non-method
          // symbols, but at erasure phase all ParamAccessors have Method flag
          val fieldName = FieldName(className, SimpleFieldName(f.name.toString))
          val valueType = toWIT(paramTpe)
          wit.FieldType(fieldName, valueType)
        }
        wit.RecordType(className, fields)
      } else if (isWasmWitResourceType(tsym)) {
        wit.ResourceType(encodeClassNamePostFlatten(tsym))
      } else if (tsym.derivesFrom(jsdefn.ComponentResultClass) && tsym.is(Sealed)) {
        val args = widened match {
          case AppliedType(_, args) => args
          case _ => Nil
        }
        val (ok, err) = (args.headOption.getOrElse(defn.UnitType), args.lift(1).getOrElse(defn.UnitType))
        wit.ResultType(toResultWIT(ok), toResultWIT(err))
      } else if (tsym.fullName.toString == "java.util.Optional") {
        val args = widened match {
          case AppliedType(_, args) => args
          case _ => Nil
        }
        val t = args.headOption.getOrElse(defn.AnyType)
        wit.OptionType(toWIT(t))
      } else if (tsym.hasAnnotation(jsdefn.WitVariantAnnot) && tsym.is(Sealed)) {
        // Sort by declaration order
        // children returns source module (term symbol) for case objects,
        // normalize to module class for encodeClassName and witVariantValueTypeOf
        val cases = tsym.children.toList.sortBy(_.span.start).map { rawChild =>
          val child = if (rawChild.is(Module) && !rawChild.isClass) rawChild.moduleClass else rawChild
          val valueType = witVariantValueTypeOf(child)
          val caseTyp = if (isWitUnitType(valueType)) {
            None
          } else {
            Some(toWIT(valueType))
          }
          wit.CaseType(encodeClassNamePostFlatten(child), caseTyp)
        }
        wit.VariantType(encodeClassNamePostFlatten(tsym), cases)
      } else {
        throw new AssertionError(s"invalid type for toWIT: $tpe (symbol: $tsym)")
      }
    }
  }

  /** Convert a Scala type to an optional WIT ValType (None for void/Unit). */
  def toResultWIT(tpe: Type): Option[wit.ValType] = atPhase(erasurePhase) {
    if (isWitUnitType(tpe)) None
    else Some(toWIT(tpe))
  }

  /** Check if a type represents Unit/void for WIT purposes.
   *  Uses symbol check instead of `toIRType` to work in pre-erasure context.
   */
  private def isWitUnitType(tpe: Type): Boolean =
    tpe.widenDealias.typeSymbol == defn.UnitClass

  /** Try to convert an array type to a WIT ListType. */
  private def toWITMaybeArray(tpe: Type): Option[wit.ValType] = {
    tpe match {
      case defn.ArrayOf(elem) =>
        Some(wit.ListType(toWIT(elem), None))
      case _ => None
    }
  }

  // Registration methods for tracking WIT exports and function types

  def registerWitExport(sym: Symbol, info: WitExportInfo): Unit =
    witExports(sym) = info

  def registerWitFunctionType(sym: Symbol, funcType: WitFunctionType): Unit =
    witFunctionTypes(sym) = funcType

  def registerWitVariantValueType(sym: Symbol, valueType: Type): Unit =
    witVariantValueTypes(sym) = valueType

  def witExportOf(sym: Symbol): Option[WitExportInfo] =
    witExports.get(sym)

  def witFunctionTypeOf(sym: Symbol): WitFunctionType = {
    witFunctionTypes.getOrElse(sym, {
      // Use pre-erasure types to preserve generic type arguments
      // (e.g., Optional[RequestOptions] instead of just Optional)
      atPhase(erasurePhase) {
        val paramTypes = sym.info.firstParamTypes
        val resultType = sym.info.resultType
        WitFunctionType(paramTypes, resultType)
      }
    })
  }

  def witVariantValueTypeOf(sym: Symbol): Type = {
    witVariantValueTypes.getOrElse(sym, {
      // Use pre-erasure types to preserve generic type arguments
      atPhase(erasurePhase) {
        // For case class fields, get the field type
        // For variant cases, get the primary constructor parameter type or Unit
        if (sym.isClass) {
          val primaryCtor = sym.asClass.primaryConstructor
          if (primaryCtor.exists) {
            val paramTypes = primaryCtor.info.firstParamTypes
            if (paramTypes.size == 1) paramTypes.head
            else defn.UnitType
          } else {
            defn.UnitType
          }
        } else {
          sym.info.widenDealias
        }
      }
    })
  }

  /** Encode a class name at the post-flatten phase.
   *
   *  `encodeClassName` relies on `fullName.mangledString` which produces different
   *  results depending on the compiler phase. Before flatten, inner classes use `.`
   *  separators (e.g., `package$.Method`), but after flatten they use `$`
   *  (e.g., `package$Method`). Since `toWIT` runs at `erasurePhase` (before flatten),
   *  we must step forward to the post-flatten phase for class name encoding.
   */
  private def encodeClassNamePostFlatten(sym: Symbol): ClassName =
    atPhase(flattenPhase.next) { encodeClassName(sym) }

  // Mapping tables for type conversion

  private lazy val unsigned2WIT: Map[Symbol, wit.ValType] = Map(
    jsdefn.WitUnsigned_UByte -> wit.U8Type,
    jsdefn.WitUnsigned_UShort -> wit.U16Type,
    jsdefn.WitUnsigned_UInt -> wit.U32Type,
    jsdefn.WitUnsigned_ULong -> wit.U64Type
  )

  /** Maps Scala type symbols to WIT value types for primitives and String.
   *  Uses symbols instead of IR types to work in pre-erasure phase context.
   */
  private lazy val primitiveSymbolWIT: Map[Symbol, wit.ValType] = Map(
    defn.BooleanClass -> wit.BoolType,
    defn.ByteClass -> wit.S8Type,
    defn.ShortClass -> wit.S16Type,
    defn.IntClass -> wit.S32Type,
    defn.LongClass -> wit.S64Type,
    defn.FloatClass -> wit.F32Type,
    defn.DoubleClass -> wit.F64Type,
    defn.CharClass -> wit.CharType,
    defn.StringClass -> wit.StringType
  )
}

/** Information about a WIT export. */
case class WitExportInfo(
    moduleName: String,
    name: String,
    signature: WitFunctionType
)(val pos: SourcePosition)

/** Function type information for WIT. */
case class WitFunctionType(
    params: List[Type],
    resultType: Type
)
