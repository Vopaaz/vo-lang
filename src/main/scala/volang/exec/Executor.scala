package volang.exec
import volang.eval.Evaluator
import volang.ast.Parser
import scala.io.Source

object Executor {
  def exec(source: String) = {
    println(Evaluator.evaluate(new Parser(source).parse).toString())
  }

  def execFile(filename: String) = {
    val source = Source.fromFile(filename).mkString
    exec(source)
  }
}
