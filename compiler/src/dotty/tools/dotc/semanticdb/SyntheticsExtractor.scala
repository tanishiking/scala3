package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.semanticdb.{javalite as jl}
import scala.jdk.CollectionConverters.*


class SyntheticsExtractor:
  import Scala3.{_, given}

  val visited = collection.mutable.HashSet[Tree]()
  private val emptyTree = jl.Tree.getDefaultInstance

  private def isTreeDefined(tree: jl.Tree): Boolean =
    tree.getSealedValueCase != jl.Tree.SealedValueCase.SEALEDVALUE_NOT_SET

  private def newOriginalTree(range: Option[jl.Range]): jl.Tree =
    val b = jl.OriginalTree.newBuilder()
    range.foreach(b.setRange)
    jl.Tree.newBuilder().setOriginalTree(b.build()).build()

  private def newIdTree(symbol: String): jl.Tree =
    jl.Tree
      .newBuilder()
      .setIdTree(jl.IdTree.newBuilder().setSymbol(symbol).build())
      .build()

  private def newSelectTree(qualifier: jl.Tree, id: Option[jl.Tree]): jl.Tree =
    val b = jl.SelectTree.newBuilder()
    if isTreeDefined(qualifier) then b.setQualifier(qualifier)
    id.foreach { tree =>
      if tree.getSealedValueCase == jl.Tree.SealedValueCase.ID_TREE then
        b.setId(tree.getIdTree)
    }
    jl.Tree.newBuilder().setSelectTree(b.build()).build()

  private def newApplyTree(function: jl.Tree, arguments: Seq[jl.Tree], properties: Int = 0): jl.Tree =
    val b = jl.ApplyTree.newBuilder().setProperties(properties)
    if isTreeDefined(function) then b.setFunction(function)
    if arguments.nonEmpty then b.addAllArguments(arguments.asJava)
    jl.Tree.newBuilder().setApplyTree(b.build()).build()

  private def newTypeApplyTree(function: jl.Tree, typeArguments: Seq[jl.Type]): jl.Tree =
    val b = jl.TypeApplyTree.newBuilder()
    if isTreeDefined(function) then b.setFunction(function)
    if typeArguments.nonEmpty then b.addAllTypeArguments(typeArguments.asJava)
    jl.Tree.newBuilder().setTypeApplyTree(b.build()).build()

  private def newSynthetic(range: Option[jl.Range], tree: jl.Tree): jl.Synthetic =
    val b = jl.Synthetic.newBuilder().setTree(tree)
    range.foreach(b.setRange)
    b.build()

  def tryFindSynthetic(tree: Tree)(using Context, SemanticSymbolBuilder, TypeOps): Option[jl.Synthetic] =
    extension (synth: jl.Synthetic)
      def toOpt: Some[jl.Synthetic] = Some(synth)

    val forSynthetic = tree match // not yet supported (for synthetics)
      case tree: Apply if isForSynthetic(tree) => true
      case tree: TypeApply if isForSynthetic(tree) => true
      case _ => false

    if visited.contains(tree) || forSynthetic then None
    else
      tree match
        case tree: TypeApply
          if tree.span.isSynthetic &&
            tree.args.forall(arg => !arg.symbol.isDefinedInSource) &&
            !tree.span.isZeroExtent &&
            (tree.fun match {
              // for `Bar[Int]` of `class Foo extends Bar[Int]`
              // we'll have `TypeTree(Select(New(AppliedTypeTree(...))), List(Int))`
              // in this case, don't register `*[Int]` to synthetics as we already have `[Int]` in source.
              case Select(New(AppliedTypeTree(_, _)), _) => false

              // for `new SomeJavaClass[Int]()`
              // there will be a synthesized default getter
              // in addition to the source derived one.
              case Select(_, name) if name.is(NameKinds.DefaultGetterName) => false
              case Select(fun, _) if fun.symbol.name.isDynamic => false
              case _ => true
            }) =>
          visited.add(tree)
          val fnTree = tree.fun match
            // Something like `List.apply[Int](1,2,3)`
            case select @ Select(qual, _) if isSyntheticName(select) =>
              newSelectTree(
                newOriginalTree(range(qual.span, tree.source)),
                Some(select.toSemanticId)
              )
            case _ =>
              newOriginalTree(
                range(tree.fun.span, tree.source)
              )
          val targs = tree.args.map(targ => targ.tpe.toSemanticType(targ.symbol)(using LinkMode.SymlinkChildren))
          newSynthetic(
            range(tree.span, tree.source),
            newTypeApplyTree(
              fnTree, targs
            )
          ).toOpt

        case tree: Apply
          if tree.args.nonEmpty &&
            tree.args.forall(arg =>
              arg.symbol.isOneOf(GivenOrImplicit) &&
              arg.span.isSynthetic
            ) =>
          newSynthetic(
            range(tree.span, tree.source),
            newApplyTree(
              tree.fun.toSemanticOriginal,
              tree.args.map(_.toSemanticTree),
              jl.SymbolInformation.Property.GIVEN.getNumber
            )
          ).toOpt

        case tree: Apply
            if tree.fun.symbol.is(Implicit) ||
              (tree.fun.symbol.name == nme.apply && tree.fun.span.isSynthetic) =>
          val pos = range(tree.span, tree.source)
          newSynthetic(
            pos,
            newApplyTree(
              tree.fun.toSemanticTree,
              arguments = List(
                newOriginalTree(pos)
              )
            )
          ).toOpt

        case _ => None

  private given TreeOps: AnyRef with
    extension (tree: Tree)
      def toSemanticTree(using Context, SemanticSymbolBuilder, TypeOps): jl.Tree =
        tree match
          case tree: Apply =>
            newApplyTree(
              tree.fun.toSemanticQualifierTree,
              tree.args.map(_.toSemanticTree)
            )
          case tree: TypeApply =>
            newTypeApplyTree(
              tree.fun.toSemanticQualifierTree,
              tree.args.map { targ =>
                targ.tpe.toSemanticType(targ.symbol)(using LinkMode.SymlinkChildren)
              }
            )
          case tree: Ident => tree.toSemanticId
          case tree: Select => tree.toSemanticId
          case _ => emptyTree

      def toSemanticQualifierTree(using Context, SemanticSymbolBuilder): jl.Tree = tree match
        case sel @ Select(qual, _) if sel.symbol.owner != qual.symbol =>
          newSelectTree(qual.toSemanticId, Some(sel.toSemanticId))
        case fun => fun.toSemanticId

      def toSemanticId(using Context, SemanticSymbolBuilder) =
        newIdTree(tree.symbol.symbolName)

      def toSemanticOriginal(using Context) =
        newOriginalTree(range(tree.span, tree.source))
  end TreeOps


  private def isForSynthetic(tree: Tree): Boolean =
    def isForComprehensionSyntheticName(select: Select): Boolean =
      select.span.toSynthetic == select.qualifier.span.toSynthetic && (
        select.name == nme.map ||
        select.name == nme.flatMap ||
        select.name == nme.withFilter ||
        select.name == nme.foreach
      )
    tree match
      case Apply(fun, _) => isForSynthetic(fun)
      case TypeApply(fun, _) => isForSynthetic(fun)
      case select: Select => isForComprehensionSyntheticName(select)
      case _ => false

  private def isSyntheticName(select: Select): Boolean =
    select.span.toSynthetic == select.qualifier.span.toSynthetic && (
      select.name == nme.apply ||
      select.name == nme.update ||
      select.name == nme.foreach ||
      select.name == nme.withFilter ||
      select.name == nme.flatMap ||
      select.name == nme.map ||
      select.name == nme.unapplySeq ||
      select.name == nme.unapply)

end SyntheticsExtractor
