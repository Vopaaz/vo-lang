package volang.repl
import volang.lexer._

object Repl {
  def startInteractive: Unit = {
    val prompt = "> "

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
}
