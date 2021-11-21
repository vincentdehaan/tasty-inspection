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

    object TagChecker extends TreeTraverser {
      val forbiddenTypes = Set[(String, String)](
        ("scala.Predef$", "String")
      )

      // TODO: only check domain package
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
        case ClassDef(_, DefDef(_, paramss, _, _), _, _, _) =>
          paramss.collect { case TermParamClause(params) => params }.flatten
            .collect { case ValDef(_, tpt, _) => tpt.tpe }
            .collect { case tpe: TypeRef => (tpe.typeSymbol.owner.fullName.toString, tpe.name.toString) } // TODO: can we do this without a tuple?
            .foreach {
              ownerAndName =>
                if(forbiddenTypes.contains(ownerAndName)) println(s"Found a forbidden type: $ownerAndName")
            }
        case tree =>
          super.traverseTree(tree)(owner)
      }
    }

    for tasty <- tastys do
      TagChecker.traverseTree(tasty.ast)(Symbol.spliceOwner)