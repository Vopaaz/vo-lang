package volang
import volang.ast.Parser
import volang.lexer._
import volang.ast._
import volang.repl.ReadLoop

object Main {
  def main(args: Array[String]) = {
    ReadLoop.startRPPL
  }
}
