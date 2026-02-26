package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.semanticdb.{javalite as jl}
import jl.{Access, Annotation, Constant, Range, Scope, Signature, SymbolInformation, Synthetic, TextDocument, Tree, Type}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import dotty.tools.dotc.semanticdb.Scala3.given
import jl.SymbolInformation.Kind.*
import dotty.tools.dotc.util.SourceFile
class SymbolInformationPrinter (symtab: PrinterSymtab):
  val notes = InfoNotes()
  val infoPrinter = InfoPrinter(notes)

  def pprintSymbolInformation(info: SymbolInformation): String =
    val sb = new StringBuilder()
    sb.append(info.getSymbol).append(" => ")
    sb.append(infoPrinter.pprint(info))
    sb.toString

  class InfoNotes:
    private val noteSymtab = mutable.Map[String, SymbolInformation]()
    def enter(info: SymbolInformation) =
      if (symtab.info(info.getSymbol).isEmpty && info.getKind != UNKNOWN_KIND)
        noteSymtab(info.getSymbol) = info

    def visit(sym: String): SymbolInformation =
      val symtabInfo = noteSymtab.get(sym).orElse(symtab.info(sym))
      symtabInfo.getOrElse {
        val displayName = if sym.isGlobal then sym.desc.value else sym
        jl.SymbolInformation.newBuilder().setSymbol(sym).setDisplayName(displayName).build()
      }
  end InfoNotes

  class InfoPrinter(notes: InfoNotes):
    private enum SymbolStyle:
      case Reference, Definition

    private def isTypeDefined(tpe: Type): Boolean =
      tpe.getSealedValueCase != jl.Type.SealedValueCase.SEALEDVALUE_NOT_SET

    def pprint(info: SymbolInformation): String =
      val sb = new StringBuilder()
      val annotStr = info.getAnnotationsList.asScala.map(pprint).mkString(" ")
      if annotStr.nonEmpty then
        sb.append(annotStr + " ")
      sb.append(accessString(if info.hasAccess then info.getAccess else Access.getDefaultInstance))
      if info.isAbstract then sb.append("abstract ")
      if info.isFinal then sb.append("final ")
      if info.isSealed then sb.append("sealed ")
      if info.isImplicit then sb.append("implicit ")
      if info.isLazy then sb.append("lazy ")
      if info.isCase then sb.append("case ")
      if info.isCovariant then sb.append("covariant ")
      if info.isContravariant then sb.append("contravariant ")
      if info.isVal then sb.append("val ")
      if info.isVar then sb.append("var ")
      if info.isStatic then sb.append("static ")
      if info.isPrimary then sb.append("primary ")
      if info.isEnum then sb.append("enum ")
      if info.isDefault then sb.append("default ")
      if info.isGiven then sb.append("given ")
      if info.isInline then sb.append("inline ")
      if info.isOpen then sb.append("open ")
      if info.isTransparent then sb.append("transparent ")
      if info.isInfix then sb.append("infix ")
      if info.isOpaque then sb.append("opaque ")
      info.getKind match
        case LOCAL => sb.append("local ")
        case FIELD => sb.append("field ")
        case METHOD => sb.append("method ")
        case CONSTRUCTOR => sb.append("ctor ")
        case MACRO => sb.append("macro ")
        case TYPE => sb.append("type ")
        case PARAMETER => sb.append("param ")
        case SELF_PARAMETER => sb.append("selfparam ")
        case TYPE_PARAMETER => sb.append("typeparam ")
        case OBJECT => sb.append("object ")
        case PACKAGE => sb.append("package ")
        case PACKAGE_OBJECT => sb.append("package object ")
        case CLASS => sb.append("class ")
        case TRAIT => sb.append("trait ")
        case INTERFACE => sb.append("interface ")
        case UNKNOWN_KIND | UNRECOGNIZED => sb.append("unknown ")
        case _ => sb.append("unknown ")
      sb.append(s"${info.getDisplayName}${info.prefixBeforeTpe}${pprint(info.getSignature)}")
      info.getOverriddenSymbolsList.asScala.toList match
        case Nil => ()
        case all => sb.append(s" <: ${all.mkString(", ")}")
      sb.toString

    private def pprintDef(info: SymbolInformation) =
      notes.enter(info)
      pprint(info.getSymbol, SymbolStyle.Definition)
    def pprintRef(sym: String): String = pprint(sym, SymbolStyle.Reference)
    private def pprintDef(sym: String): String = pprint(sym, SymbolStyle.Definition)
    private def pprint(sym: String, style: SymbolStyle): String =
      val info = notes.visit(sym)
      style match
        case SymbolStyle.Reference =>
          info.getDisplayName
        case SymbolStyle.Definition =>
          pprint(info)


    private def pprint(sig: Signature): String =
      sig.getSealedValueCase match
        case jl.Signature.SealedValueCase.CLASS_SIGNATURE =>
          val cls = sig.getClassSignature
          val tparams = if cls.hasTypeParameters then Some(cls.getTypeParameters) else None
          val parents = cls.getParentsList.asScala.toList
          val self = if cls.hasSelf then cls.getSelf else Type.getDefaultInstance
          val decls = if cls.hasDeclarations then Some(cls.getDeclarations) else None
          val sb = new StringBuilder()
          if (tparams.infos.nonEmpty)
            sb.append(tparams.infos.map(pprintDef).mkString("[", ", ", "] "))
          if (parents.nonEmpty)
            sb.append(parents.map(pprint).mkString("extends ", " with ", " "))
          if (isTypeDefined(self) || decls.infos.nonEmpty) {
            val selfStr = if isTypeDefined(self) then s"self: ${pprint(self)} =>" else ""
            val declsStr = if (decls.infos.nonEmpty) s"+${decls.infos.length} decls" else ""
            sb.append(s"{ ${selfStr} ${declsStr} }")
          }
          sb.toString
        case jl.Signature.SealedValueCase.METHOD_SIGNATURE =>
          val method = sig.getMethodSignature
          val tparams = if method.hasTypeParameters then Some(method.getTypeParameters) else None
          val paramss = method.getParameterListsList.asScala.toList
          val res = if method.hasReturnType then method.getReturnType else Type.getDefaultInstance
          val sb = new StringBuilder()
          if (tparams.infos.nonEmpty)
            sb.append(tparams.infos.map(pprintDef).mkString("[", ", ", "]"))
          paramss.foreach { params =>
            val paramsStr = params.infos.map(pprintDef).mkString("(", ", ", ")")
            sb.append(paramsStr)
          }
          sb.append(s": ${pprint(res)}")
          sb.toString
        case jl.Signature.SealedValueCase.TYPE_SIGNATURE =>
          val sigType = sig.getTypeSignature
          val tparams = if sigType.hasTypeParameters then Some(sigType.getTypeParameters) else None
          val lo = if sigType.hasLowerBound then sigType.getLowerBound else Type.getDefaultInstance
          val hi = if sigType.hasUpperBound then sigType.getUpperBound else Type.getDefaultInstance
          val sb = new StringBuilder()
          if (tparams.infos.nonEmpty)
            sb.append(tparams.infos.map(pprintDef).mkString("[", ", ", "]"))
          if (lo == hi) {
            sb.append(s" = ${pprint(lo)}")
          } else {
            if !isTypeRefTo(lo, "scala/Nothing#") then sb.append(s" >: ${pprint(lo)}")
            if !isTypeRefTo(hi, "scala/Any#") && !isTypeRefTo(hi, "java/lang/Object#") then
              sb.append(s" <: ${pprint(hi)}")
          }
          sb.toString
        case jl.Signature.SealedValueCase.VALUE_SIGNATURE =>
          val value = sig.getValueSignature
          val tpe = if value.hasTpe then value.getTpe else Type.getDefaultInstance
          pprint(tpe)
        case _ =>
          "<?>"

    private def isTypeRefTo(tpe: Type, symbol: String): Boolean =
      tpe.getSealedValueCase == jl.Type.SealedValueCase.TYPE_REF &&
      !tpe.getTypeRef.hasPrefix &&
      tpe.getTypeRef.getSymbol == symbol &&
      tpe.getTypeRef.getTypeArgumentsCount == 0

    protected def pprint(tpe: Type): String = {
      def isSingletonLikeType(tpe: Type): Boolean = tpe.getSealedValueCase match
        case jl.Type.SealedValueCase.SINGLE_TYPE | jl.Type.SealedValueCase.THIS_TYPE | jl.Type.SealedValueCase.SUPER_TYPE => true
        case _ => false

      def prefix(tpe: Type): String = tpe.getSealedValueCase match
        case jl.Type.SealedValueCase.TYPE_REF =>
          val ref = tpe.getTypeRef
          val pre = if ref.hasPrefix then ref.getPrefix else Type.getDefaultInstance
          val sym = ref.getSymbol
          val args = ref.getTypeArgumentsList.asScala.toList
          val preStr =
            if isSingletonLikeType(pre) then s"${prefix(pre)}."
            else if pre == Type.getDefaultInstance then ""
            else s"${prefix(pre)}#"
          val argsStr = if (args.nonEmpty) args.map(normal).mkString("[", ", ", "]") else ""
          s"${preStr}${pprintRef(sym)}${argsStr}"
        case jl.Type.SealedValueCase.SINGLE_TYPE =>
          val single = tpe.getSingleType
          val pre = if single.hasPrefix then single.getPrefix else Type.getDefaultInstance
          val sym = single.getSymbol
          if pre == Type.getDefaultInstance then pprintRef(sym)
          else s"${prefix(pre)}.${pprintRef(sym)}"
        case jl.Type.SealedValueCase.THIS_TYPE =>
          s"${pprintRef(tpe.getThisType.getSymbol)}.this"
        case jl.Type.SealedValueCase.SUPER_TYPE =>
          val sup = tpe.getSuperType
          val pre = if sup.hasPrefix then sup.getPrefix else Type.getDefaultInstance
          s"${prefix(pre)}.super[${pprintRef(sup.getSymbol)}]"
        case jl.Type.SealedValueCase.CONSTANT_TYPE =>
          val constType = tpe.getConstantType
          val const = if constType.hasConstant then constType.getConstant else Constant.getDefaultInstance
          pprint(const)
        case jl.Type.SealedValueCase.INTERSECTION_TYPE =>
          tpe.getIntersectionType.getTypesList.asScala.toList.map(normal).mkString(" & ")
        case jl.Type.SealedValueCase.UNION_TYPE =>
          tpe.getUnionType.getTypesList.asScala.toList.map(normal).mkString(" | ")
        case jl.Type.SealedValueCase.WITH_TYPE =>
          tpe.getWithType.getTypesList.asScala.toList.map(normal).mkString(" with ")
        case jl.Type.SealedValueCase.STRUCTURAL_TYPE =>
          val structural = tpe.getStructuralType
          val utpe = if structural.hasTpe then structural.getTpe else Type.getDefaultInstance
          val decls = if structural.hasDeclarations then Some(structural.getDeclarations) else None
          val declsStr =
            if (decls.infos.nonEmpty)
              s"{ ${decls.infos.map(pprintDef).mkString("; ")} }"
            else "{}"
          s"${normal(utpe)} ${declsStr}"
        case jl.Type.SealedValueCase.ANNOTATED_TYPE =>
          val annotated = tpe.getAnnotatedType
          val anns = annotated.getAnnotationsList.asScala.toList
          val utpe = if annotated.hasTpe then annotated.getTpe else Type.getDefaultInstance
          s"${normal(utpe)} ${anns.map(pprint).mkString(" ")}"
        case jl.Type.SealedValueCase.EXISTENTIAL_TYPE =>
          val existential = tpe.getExistentialType
          val utpe = if existential.hasTpe then existential.getTpe else Type.getDefaultInstance
          val decls = if existential.hasDeclarations then Some(existential.getDeclarations) else None
          val sdecls = decls.infos.map(pprintDef).mkString("; ")
          val sutpe = normal(utpe)
          s"${sutpe} forSome { ${sdecls} }"
        case jl.Type.SealedValueCase.UNIVERSAL_TYPE =>
          val universal = tpe.getUniversalType
          val tparams = if universal.hasTypeParameters then Some(universal.getTypeParameters) else None
          val utpe = if universal.hasTpe then universal.getTpe else Type.getDefaultInstance
          val params = tparams.infos.map(_.getDisplayName).mkString("[", ", ", "]")
          val resType = normal(utpe)
          s"${params} => ${resType}"
        case jl.Type.SealedValueCase.BY_NAME_TYPE =>
          val byName = tpe.getByNameType
          val utpe = if byName.hasTpe then byName.getTpe else Type.getDefaultInstance
          s"=> ${normal(utpe)}"
        case jl.Type.SealedValueCase.REPEATED_TYPE =>
          val repeated = tpe.getRepeatedType
          val utpe = if repeated.hasTpe then repeated.getTpe else Type.getDefaultInstance
          s"${normal(utpe)}*"
        case jl.Type.SealedValueCase.MATCH_TYPE =>
          val mt = tpe.getMatchType
          val scrutinee = if mt.hasScrutinee then mt.getScrutinee else Type.getDefaultInstance
          val casesStr = mt.getCasesList.asScala.toList.map { caseType =>
            s"${pprint(caseType.getKey)} => ${pprint(caseType.getBody)}"
          }.mkString(", ")
          s"${pprint(scrutinee)} match { ${casesStr} }"
        case jl.Type.SealedValueCase.LAMBDA_TYPE =>
          val lambda = tpe.getLambdaType
          val tparams = if lambda.hasParameters then Some(lambda.getParameters) else None
          val res = if lambda.hasReturnType then lambda.getReturnType else Type.getDefaultInstance
          val params = tparams.infos.map(_.getDisplayName).mkString("[", ", ", "]")
          val resType = normal(res)
          s"$params =>> $resType"
        case _ =>
          "<?>"

      def normal(tpe: Type): String =
        if isSingletonLikeType(tpe) then s"${prefix(tpe)}.type"
        else prefix(tpe)

      normal(tpe)
    }

    private def pprint(ann: Annotation): String =
      val tpe = if ann.hasTpe then ann.getTpe else Type.getDefaultInstance
      if isTypeDefined(tpe) then s"@${pprint(tpe)}" else "@<?>"

    protected def pprint(const: Constant): String = const.getSealedValueCase match {
        case jl.Constant.SealedValueCase.SEALEDVALUE_NOT_SET =>
          "<?>"
        case jl.Constant.SealedValueCase.UNIT_CONSTANT =>
          "()"
        case jl.Constant.SealedValueCase.BOOLEAN_CONSTANT =>
          if const.getBooleanConstant.getValue then "true" else "false"
        case jl.Constant.SealedValueCase.BYTE_CONSTANT =>
          const.getByteConstant.getValue.toByte.toString
        case jl.Constant.SealedValueCase.SHORT_CONSTANT =>
          const.getShortConstant.getValue.toShort.toString
        case jl.Constant.SealedValueCase.CHAR_CONSTANT =>
          s"'${const.getCharConstant.getValue.toChar.toString}'"
        case jl.Constant.SealedValueCase.INT_CONSTANT =>
          const.getIntConstant.getValue.toString
        case jl.Constant.SealedValueCase.LONG_CONSTANT =>
          s"${const.getLongConstant.getValue.toString}L"
        case jl.Constant.SealedValueCase.FLOAT_CONSTANT =>
          s"${const.getFloatConstant.getValue.toString}f"
        case jl.Constant.SealedValueCase.DOUBLE_CONSTANT =>
          const.getDoubleConstant.getValue.toString
        case jl.Constant.SealedValueCase.STRING_CONSTANT =>
          "\"" + const.getStringConstant.getValue + "\""
        case jl.Constant.SealedValueCase.NULL_CONSTANT =>
          "null"
      }

    private def accessString(access: Access): String =
      access.getSealedValueCase match
        case jl.Access.SealedValueCase.SEALEDVALUE_NOT_SET => ""
        case jl.Access.SealedValueCase.PUBLIC_ACCESS => ""
        case jl.Access.SealedValueCase.PRIVATE_ACCESS => "private "
        case jl.Access.SealedValueCase.PROTECTED_ACCESS => "protected "
        case jl.Access.SealedValueCase.PRIVATE_THIS_ACCESS => "private[this] "
        case jl.Access.SealedValueCase.PROTECTED_THIS_ACCESS => "protected[this] "
        case jl.Access.SealedValueCase.PRIVATE_WITHIN_ACCESS =>
          s"private[${access.getPrivateWithinAccess.getSymbol}] "
        case jl.Access.SealedValueCase.PROTECTED_WITHIN_ACCESS =>
          s"protected[${access.getProtectedWithinAccess.getSymbol}] "
    extension (scope: Scope)
      private def infos: List[SymbolInformation] =
        if (scope.getSymlinksList.asScala.nonEmpty)
          scope.getSymlinksList.asScala.map(symbol => jl.SymbolInformation.newBuilder().setSymbol(symbol).build()).toList
        else
          scope.getHardlinksList.asScala.toList

    extension (scope: Option[Scope])
      private def infos: List[SymbolInformation] = scope match {
        case Some(scope) => scope.infos
        case None => Nil
      }
  end InfoPrinter
end SymbolInformationPrinter

extension (info: SymbolInformation)
  def prefixBeforeTpe: String = {
    info.getKind match {
      case LOCAL | FIELD | PARAMETER | SELF_PARAMETER | UNKNOWN_KIND | UNRECOGNIZED =>
        ": "
      case METHOD | CONSTRUCTOR | MACRO | TYPE | TYPE_PARAMETER | OBJECT | PACKAGE |
          PACKAGE_OBJECT | CLASS | TRAIT | INTERFACE =>
        " "
      case _ =>
        ": "
    }
  }

trait PrinterSymtab:
  def info(symbol: String): Option[SymbolInformation]
object PrinterSymtab:
  def fromTextDocument(doc: TextDocument): PrinterSymtab =
    val map = doc.getSymbolsList.asScala.map(info => (info.getSymbol, info)).toMap
    new PrinterSymtab {
      override def info(symbol: String): Option[SymbolInformation] = map.get(symbol)
    }

def processRange(sb: StringBuilder, range: Range): Unit =
  sb.append('[')
    .append(range.getStartLine).append(':').append(range.getStartCharacter)
    .append("..")
    .append(range.getEndLine).append(':').append(range.getEndCharacter)
    .append("):")



class SyntheticPrinter(symtab: PrinterSymtab, source: SourceFile) extends SymbolInformationPrinter(symtab):

  def pprint(synth: Synthetic): String =
    val sb = new StringBuilder()
    val notes = InfoNotes()
    val treePrinter = TreePrinter(source, (if synth.hasRange then Some(synth.getRange) else None), notes)

    (if synth.hasRange then Some(synth.getRange) else None) match
      case Some(range) =>
        processRange(sb, range)
        sb.append(source.substring(range))
      case None =>
        sb.append("[):")
    sb.append(" => ")
    sb.append(treePrinter.pprint(synth.getTree))
    sb.toString

  extension (source: SourceFile)
    private def substring(range: Option[jl.Range]): String =
      range match
        case Some(range) => source.substring(range)
        case None => ""
    private def substring(range: jl.Range): String =
      /** get the line length of a given line */
      def lineLength(line: Int): Int =
        val isLastLine = source.lineToOffsetOpt(line).nonEmpty && source.lineToOffsetOpt(line + 1).isEmpty
        if isLastLine then source.content.length - source.lineToOffset(line) - 1
        else source.lineToOffset(line + 1) - source.lineToOffset(line) - 1 // -1 for newline char

      val start = source.lineToOffset(range.getStartLine) +
        math.min(range.getStartCharacter, lineLength(range.getStartLine))
      val end = source.lineToOffset(range.getEndLine) +
        math.min(range.getEndCharacter, lineLength(range.getEndLine))
      new String(source.content, start, end - start)


  // def pprint(tree: jl.Tree, range: Option[Range]): String =
  class TreePrinter(source: SourceFile, originalRange: Option[Range], notes: InfoNotes) extends InfoPrinter(notes):
    def pprint(tree: Tree): String =
      val sb = new StringBuilder()
      processTree(tree)(using sb)
      sb.toString


    private def rep[T](xs: Seq[T], seq: String)(f: T => Unit)(using sb: StringBuilder): Unit =
      xs.zipWithIndex.foreach { (x, i) =>
        if i != 0 then sb.append(seq)
        f(x)
      }

    private def processTree(tree: Tree)(using sb: StringBuilder): Unit =
      tree.getSealedValueCase match {
        case jl.Tree.SealedValueCase.APPLY_TREE =>
          val apply = tree.getApplyTree
          val function = if apply.hasFunction then apply.getFunction else Tree.getDefaultInstance
          processTree(function)
          sb.append("(")
          rep(apply.getArgumentsList.asScala.toList, ", ")(processTree)
          sb.append(")")
        case jl.Tree.SealedValueCase.FUNCTION_TREE =>
          val fn = tree.getFunctionTree
          val parameters = fn.getParametersList.asScala.toList.map(id =>
            jl.Tree.newBuilder().setIdTree(id).build()
          )
          val body = if fn.hasBody then fn.getBody else Tree.getDefaultInstance
          sb.append("{")
          sb.append("(")
          rep(parameters, ", ")(processTree)
          sb.append(") =>")
          processTree(body)
          sb.append("}")
        case jl.Tree.SealedValueCase.ID_TREE =>
          sb.append(pprintRef(tree.getIdTree.getSymbol))
        case jl.Tree.SealedValueCase.LITERAL_TREE =>
          val lit = tree.getLiteralTree
          val constant = if lit.hasConstant then lit.getConstant else Constant.getDefaultInstance
          sb.append(pprint(constant))
        case jl.Tree.SealedValueCase.MACRO_EXPANSION_TREE =>
          val macroTree = tree.getMacroExpansionTree
          val tpe = if macroTree.hasTpe then macroTree.getTpe else Type.getDefaultInstance
          sb.append("(`macro-expandee` : `")
          sb.append(pprint(tpe))
          sb.append(")")
        case jl.Tree.SealedValueCase.ORIGINAL_TREE =>
          val original = tree.getOriginalTree
          val range = if original.hasRange then Some(original.getRange) else None
          if (range == originalRange && originalRange.nonEmpty) then
            sb.append("*")
          else
            sb.append("orig(")
            sb.append(source.substring(range))
            sb.append(")")
        case jl.Tree.SealedValueCase.SELECT_TREE =>
          val select = tree.getSelectTree
          val qualifier = if select.hasQualifier then select.getQualifier else Tree.getDefaultInstance
          processTree(qualifier)
          sb.append(".")
          if select.hasId then
            processTree(jl.Tree.newBuilder().setIdTree(select.getId).build())
        case jl.Tree.SealedValueCase.TYPE_APPLY_TREE =>
          val typeApply = tree.getTypeApplyTree
          val function = if typeApply.hasFunction then typeApply.getFunction else Tree.getDefaultInstance
          val typeArguments = typeApply.getTypeArgumentsList.asScala.toList
          processTree(function)
          sb.append("[")
          rep(typeArguments, ", ")((t) => sb.append(pprint(t)))
          sb.append("]")
        case _ =>
          sb.append("<?>")
      }


end SyntheticPrinter
