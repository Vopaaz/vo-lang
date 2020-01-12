package volang
import volang.ast.Parser
import volang.lexer._
import volang.ast._
import volang.exec._
import java.io.{StringBufferInputStream, ByteArrayOutputStream}

object Main {
  def main(args: Array[String]) = {
    if (args.length > 1) {
      throw new Exception("Only zero or one argument is supported.")
    }

    if (args.isEmpty) {
      ReadLoop.startREPL
    } else {
      args.head match {
        case "-p" => ReadLoop.startRPPL
        case "-l" => ReadLoop.startRLPL
        case _    => Executor.execFile(args.head)
      }
    }
  }
}
