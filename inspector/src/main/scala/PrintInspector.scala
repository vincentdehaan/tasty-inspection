import dotty.tools.dotc.ast.tpd

import scala.quoted.*
import scala.tasty.inspector.*
import java.io.File
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context

object PrintInspector:
  def main(args: Array[String]): Unit =
    TastyInspector.inspectTastyFiles(listTastyFiles("example").toList)(new MyInspector)

  def listTastyFiles(path: String): Seq[String] =
    val d = new File(path)
    if(d.exists && d.isDirectory) {
      d.listFiles.flatMap {
        file =>
          if(file.isDirectory) listTastyFiles(file.getAbsolutePath)
          else Some(file.getAbsolutePath).filter(_.endsWith(".tasty"))
      }
    } else Seq()

class MyInspector extends Inspector:

  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import quotes.reflect.*

    enum TypeAlias:
      case SimpleAlias(baseType: String)
      case TaggedAlias(baseType: String, tagName: String)

    import TypeAlias.*

    def tptToString(tpt: TypeTree): String = tpt.tpe match {
      case tpe: TypeRef => tpe.typeSymbol.owner.fullName.toString + "." + tpe.name.toString
      case _ => "UNSUPPORTED TYPE TREE"
    }

    object TypeAliasFinder extends TreeAccumulator[Seq[(String, TypeAlias)]] {
      def foldTree(aliases: Seq[(String, TypeAlias)], tree: Tree)(owner: Symbol): Seq[(String, TypeAlias)] = tree match {
        case TypeDef(name, tpt @ AppliedTypeTree(TypeIdent("@@"), List(Ident(baseType), Ident(tagName)))) => // TypeIdent("@@") yields a compiler warning. Why??
          println(s"typedef $name $baseType $tagName")

          Seq((name, TaggedAlias(baseType, tagName)))
          foldOverTree(aliases ++ Seq((name, TaggedAlias(baseType, tagName))), tree)(owner)
        case TypeDef(name, ti @ TypeIdent(baseType)) =>
          println(s"typedef $name $baseType")
          println(tptToString(ti))
          Seq((name, SimpleAlias(baseType)))
          foldOverTree(aliases ++ Seq((name, SimpleAlias(baseType))), tree)(owner)
        case _ => foldOverTree(aliases, tree)(owner)
      }
    }

    class TagChecker(typeAliases: Map[String, TypeAlias]) extends TreeTraverser {
      val forbiddenTypes = Set(
        "scala.Predef$.String"
      )

      // TODO: only check domain package
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
        case ClassDef(_, DefDef(_, paramss, _, _), _, _, _) =>
          paramss.collect { case TermParamClause(params) => params }.flatten
            .collect { case ValDef(_, tpt, _) => tptToString(tpt) }
            .foreach {
              typeName =>
                typeAliases.get(typeName) match {
                  case Some(SimpleAlias(baseType)) => forbiddenTypes.contains(baseType)
                  case Some(_) => false
                  case None =>
                }
                if(forbiddenTypes.contains(typeName)) println(s"Found a forbidden type: $typeName")
            }
        case tree =>
          super.traverseTree(tree)(owner)
      }
    }

    //val aliases = TypeAliasFinder.foldTrees(Seq(), tastys.map((t: Tasty[quotes.type]) => t.ast))(Symbol.spliceOwner) // TODO: the compiler does not want this

    val aliases = (for tasty <- tastys
      yield TypeAliasFinder.foldTree(Seq(), tasty.ast)(Symbol.spliceOwner)).flatten

    aliases.foreach(println)

    for tasty <- tastys do
      (new TagChecker(aliases.toMap)).traverseTree(tasty.ast)(Symbol.spliceOwner)

   /* for tasty <- tastys do
      println("=======")
      println(tasty.ast)
      println()
      println(scala.quoted.runtime.impl.printers.Extractors.showTree(tasty.ast))
      println()*/