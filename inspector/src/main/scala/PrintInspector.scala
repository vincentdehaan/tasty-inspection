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


    object Traverser extends TreeTraverser {
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
        case ClassDef(a, DefDef(_, paramss, _, _), c, d, e) =>
          paramss.collect {
            case TermParamClause(params) => println(s"term: $params")
              params
          }.flatten
          .collect {
            case ValDef(_, tpt, _) =>
              println(tpt.getClass.getName)
              tpt.tpe match {
                case TermRef(prefix, name) => println("termref" + prefix + name)
                case tpe: TypeRef => println("typeref")
                  println(tpe.qualifier)
                  println(tpe.typeSymbol)
                  println(tpe.typeSymbol.owner.fullName)

              //case TypeIdent(name) => println("ident" + name)
              //case TypeSelect(qual, name) => println("select" + qual + name)
            }
          }
          //println(s"$params")
        //case y @ Ident(x) => println(y.toString + "   " + y.tpe)
        case tree =>
          //println(owner.toString + owner.name)
          super.traverseTree(tree)(owner)
      }
    }

    for tasty <- tastys do
      //println(tasty.ast)
      val ast = tasty.ast
      val shown = ast.show
      //println(tasty.ast.show)
      println("========")
      Traverser.traverseTree(tasty.ast)(Symbol.spliceOwner)