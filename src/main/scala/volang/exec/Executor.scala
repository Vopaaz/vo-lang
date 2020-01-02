package volang.exec
import volang.eval.Evaluator
import volang.ast.Parser

object Executor {
  def exec(source: String) = {
    println(Evaluator.evaluate(new Parser(source).parse).toString())
  }
}
