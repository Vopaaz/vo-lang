package volang
import volang.ast.Parser
import volang.lexer._
import volang.ast._
import volang.exec._

object Main {
  def main(args: Array[String]) = {
    // ReadLoop.startREPL
    val input = """
    let ceiling1 = func(x) {
      if (x > 1) {
        ceiling1(x - 1)
      } else {
        x
      }
    }
    ceiling1(10)
    """
    Executor.exec(input)
  }
}
