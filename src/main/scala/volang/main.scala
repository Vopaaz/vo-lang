package volang
import volang.repl.Repl
import volang.ast.Parser
import volang.lexer._
import volang.ast._

object Main {
  def main(args: Array[String]) = {
    val input = """
    f()
    """
    // f(1)
    // f(a, 1+2)
    val stmt = new Parser(input).parse.statements(0)
    println(stmt.toString())
  }
}
