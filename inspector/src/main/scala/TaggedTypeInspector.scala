import dotty.tools.dotc.ast.tpd

import scala.quoted.*
import scala.tasty.inspector.*
import java.io.File
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context

object TaggedTypeInspector:
  def main(args: Array[String]): Unit =
    TastyInspector.inspectTastyFiles(listTastyFiles("example").toList)(new TaggedTypeInspector)

  def listTastyFiles(path: String): Seq[String] =
    val d = new File(path)
    if(d.exists && d.isDirectory) {
      d.listFiles.flatMap {
        file =>
          if(file.isDirectory) listTastyFiles(file.getAbsolutePath)
          else Some(file.getAbsolutePath).filter(_.endsWith(".tasty"))
      }
    } else Seq()

class TaggedTypeInspector extends Inspector:

  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import quotes.reflect.*

    def tptToString(tpt: TypeTree): String = tpt.tpe match {
      case tpe: TypeRef => tpe.typeSymbol.owner.fullName.toString + "." + tpe.name.toString
      case _ => "UNSUPPORTED TYPE TREE"
    }

    object TypeAliasFinder extends TreeAccumulator[Seq[(String, String)]] {
      def foldTree(aliases: Seq[(String, String)], tree: Tree)(owner: Symbol): Seq[(String, String)] = tree match {
        case TypeDef(name, ti @ TypeIdent(_)) =>
          foldOverTree(aliases ++ Seq((owner.fullName.toString + "." + name, tptToString(ti))), tree)(owner)
        case _ => foldOverTree(aliases, tree)(owner)
      }
    }

    class TagChecker(typeAliases: Map[String, String]) extends TreeTraverser {
      val forbiddenTypes = Set(
        "scala.Predef$.String",
        "scala.Int" // TODO: add other primitives
      )

      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
        case ClassDef(_, DefDef(_, paramss, _, _), _, _, _) =>
          paramss.collect { case TermParamClause(params) => params }.flatten
            .collect { case ValDef(_, tpt, _) => tptToString(tpt) }
            .foreach {
              typeName =>
                typeAliases.get(typeName) match {
                  case Some(alias) if forbiddenTypes.contains(alias) =>
                    println(s"Found a forbidden type alias: $typeName is actually $alias")
                  case Some(_) =>
                  case None =>
                    if(forbiddenTypes.contains(typeName))
                      println(s"Found a forbidden type: $typeName")
                }
            }
        case tree =>
          super.traverseTree(tree)(owner)
      }
    }

    val aliases = (for tasty <- tastys
      yield TypeAliasFinder.foldTree(Seq(), tasty.ast)(Symbol.spliceOwner)).flatten

    for tasty <- tastys do
      (new TagChecker(aliases.toMap)).traverseTree(tasty.ast)(Symbol.spliceOwner)