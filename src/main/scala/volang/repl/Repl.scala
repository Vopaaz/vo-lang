package volang.repl
import volang.lexer._
import volang.ast.Parser
import volang.eval.Evaluator

object ReadLoop {
  private val prompt = "> "
  def startRLPL: Unit = {

    /** Read-Lex-Print-Loop
      */
    while (true) {
      print(prompt)
      Console.flush()

      val s = io.StdIn.readLine()
      val l = new Lexer(s)
      while (l.hasNext) {
        println(l.nextToken.toString)
      }
    }
  }

  def startRPPL: Unit = {

    /** Read-Parse-Print-Loop
      */
    while (true) {
      print(prompt)
      Console.flush()

      val s          = io.StdIn.readLine()
      val statements = new Parser(s).parse.statements
      statements.foreach(println)
    }
  }

  def startREPL: Unit = {

    /** Read-Evaluation-Print-Loop
      */
    while (true) {
      print(prompt)
      Console.flush()

      val s    = io.StdIn.readLine()
      val root = new Parser(s).parse
      println(Evaluator.evaluate(root))
    }
  }
}
