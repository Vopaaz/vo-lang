package volang
import volang.ast.Parser
import volang.lexer._
import volang.ast._
import volang.exec._

object Main {
  def main(args: Array[String]) = {
    ReadLoop.startREPL
  }
}
