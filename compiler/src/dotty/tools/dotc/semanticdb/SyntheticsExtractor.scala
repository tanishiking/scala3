package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.{semanticdb => s}

import scala.collection.mutable

class SyntheticsExtractor:
  import Scala3.{_, given}

  val visited = collection.mutable.HashSet[Tree]()

  def tryFindSynthetic(tree: Tree)(using Context, SemanticSymbolBuilder, TypeOps): Option[s.Synthetic] =
    extension (synth: s.Synthetic)
      def toOpt: Some[s.Synthetic] = Some(synth)

    // println(s"${tree.span.isSynthetic}: ${tree}")
    if visited.contains(tree) then None
    else
      if tree.span.isSynthetic || isInventedGiven(tree) then
        tree match
          case tree: Apply if isForSynthetic(tree) =>
            None // not yet supported (for synthetics)
          case tree: TypeApply
            if tree.args.forall(arg => !arg.symbol.is(Scala2x)) &&
              !tree.span.isZeroExtent =>
            // println(tree)
            // println(tree.args.foreach(arg => println(arg.symbol.flagsString)))
            // println("===")
            // println(tree)
            // println(tree.symbol.flagsString)
            // println(tree.fun.symbol.flagsString)
            // tree.args.foreach(targ => println(targ.symbol.flagsString))
            visited.add(tree)
            val fnTree = tree.fun match
              // Something like `List.apply[Int](1,2,3)`
              case select @ Select(qual, _) if isSyntheticName(select) =>
                s.SelectTree(
                  s.OriginalTree(range(qual.span, tree.source)),
                  Some(select.toSemanticId)
                )
              case _ =>
                s.OriginalTree(
                  range(tree.fun.span, tree.source)
                )
            val targs = tree.args.map(targ => targ.tpe.toSemanticType(targ.symbol)(using LinkMode.SymlinkChildren))
            s.Synthetic(
              range(tree.span, tree.source),
              s.TypeApplyTree(
                fnTree, targs
              )
            ).toOpt
          case tree: Apply
            if tree.args.nonEmpty &&
              tree.args.forall(arg =>
                arg.symbol.isOneOf(GivenOrImplicit) &&
                arg.span.isSynthetic
              ) =>
            s.Synthetic(
              range(tree.span, tree.source),
              s.ApplyTree(
                tree.fun.toSemanticOriginal,
                tree.args.map(_.toSemanticTree)
              )
            ).toOpt

          case tree: Apply if tree.fun.symbol.is(Implicit) =>
            val pos = range(tree.span, tree.source)
            s.Synthetic(
              pos,
              s.ApplyTree(
                tree.fun.toSemanticTree,
                arguments = List(
                  s.OriginalTree(pos)
                )
              )
            ).toOpt

          // Anonymous context parameter
          case tree: ValDef if tree.symbol.is(Given) =>
            s.Synthetic(
              range(tree.span, tree.source),
              tree.toSemanticId
            ).toOpt
          case _ => None
      else
        tree match
          case tree @ Apply(tapp @ TypeApply(select: Select, targs), args) if !visited.contains(tapp) =>
            val pos = range(tree.span, tree.source)
            val stargs = targs.map(targ => targ.tpe.toSemanticType(targ.symbol)(using LinkMode.SymlinkChildren))
            s.Synthetic(
              pos,
              s.ApplyTree(
                s.TypeApplyTree(
                  select.toSemanticQualifierTree,
                  stargs,
                ),
                List(
                  s.OriginalTree(pos)
                )
              )
            ).toOpt
          case _ => None

  private given TreeOps: AnyRef with
    extension (tree: Tree)
      def toSemanticTree(using Context, SemanticSymbolBuilder, TypeOps): s.Tree =
        tree match
          case tree: Apply =>
            s.ApplyTree(
              tree.fun.toSemanticQualifierTree,
              tree.args.map(_.toSemanticTree)
            )
          case tree: TypeApply =>
            s.TypeApplyTree(
              tree.fun.toSemanticQualifierTree,
              tree.args.map { targ =>
                targ.tpe.toSemanticType(targ.symbol)(using LinkMode.SymlinkChildren)
              }
            )
          case tree: Ident => tree.toSemanticId
          case tree: Select => tree.toSemanticId
          case _ => s.Tree.defaultInstance

      def toSemanticQualifierTree(using Context, SemanticSymbolBuilder): s.Tree = tree match
        case sel @ Select(qual, _) if sel.symbol.owner != qual.symbol =>
          s.SelectTree(qual.toSemanticId, Some(sel.toSemanticId))
        case fun => fun.toSemanticId

      def toSemanticId(using Context, SemanticSymbolBuilder) =
        s.IdTree(tree.symbol.symbolName)

      def toSemanticOriginal(using Context) =
        s.OriginalTree(range(tree.span, tree.source))
  end TreeOps

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

end SyntheticsExtractor
