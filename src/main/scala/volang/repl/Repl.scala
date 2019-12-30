package volang.repl
import volang.lexer._
import volang.ast.Parser
import volang.eval.Evaluator
import scala.util.control.Breaks._

object ReadLoop {
  private val prompt = "> "
  private val exit   = List(":q", ":quit", ":exit")
  private val message =
    """
            _______         _        _______  _        _______
  |\     /|(  ___  )       ( \      (  ___  )( (    /|(  ____ \
  | )   ( || (   ) |       | (      | (   ) ||  \  ( || (    \/
  | |   | || |   | | _____ | |      | (___) ||   \ | || |
  ( (   ) )| |   | |(_____)| |      |  ___  || (\ \) || | ____
   \ \_/ / | |   | |       | |      | (   ) || | \   || | \_  )
    \   /  | (___) |       | (____/\| )   ( || )  \  || (___) |
     \_/   (_______)       (_______/|/     \||/    )_)(_______)

This is Vo programming language. See more information at https://github.com/Vopaaz/vo-lang.
Type ":q", ":quit" or ":exit" to quit the interpreter."""
  def startRLPL: Unit = {

    /** Read-Lex-Print-Loop
      */
    println(message)
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
    println(message)
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
    println(message)
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
