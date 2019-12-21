package volang
import volang.repl.Repl
import volang.ast.Parser
import volang.lexer.Lexer

object Main {
  def main(args: Array[String]) = {
    // Repl.startInteractive
    val input      = """
    -5


    -5


    -5


    """
    val statements = new Parser(input).parse.statements.foreach(print)
    val l          = new Lexer(input)
    while (l.hasNext) {
      println(l.nextToken)
    }
  }
}
