package volang.repl
import volang.lexer._
import volang.ast.Parser
import volang.eval.Evaluator
import scala.util.control.Breaks._

object ReadLoop {
  private val prompt = "> "
  private val exit   = List(":q", ":quit", ":exit")
  def startRLPL: Unit = {

    /** Read-Lex-Print-Loop
      */
    breakable {
      while (true) {
        print(prompt)
        Console.flush()

        val s = io.StdIn.readLine()
        if (exit.contains(s.trim())) {
          break
        }
        val l = new Lexer(s)
        while (l.hasNext) {
          println(l.nextToken.toString)
        }
      }
    }
  }

  def startRPPL: Unit = {

    /** Read-Parse-Print-Loop
      */
    breakable {
      while (true) {
        print(prompt)
        Console.flush()

        val s = io.StdIn.readLine()
        if (exit.contains(s.trim())) {
          break
        }
        val statements = new Parser(s).parse.statements
        statements.foreach(println)
      }
    }
  }

  def startREPL: Unit = {

    /** Read-Evaluation-Print-Loop
      */
    breakable {
      while (true) {
        print(prompt)
        Console.flush()

        val s = io.StdIn.readLine()
        if (exit.contains(s.trim())) {
          break
        }
        val root = new Parser(s).parse
        println(Evaluator.evaluate(root))
      }
    }
  }
}
