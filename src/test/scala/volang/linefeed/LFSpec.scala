package volang.linefeed

import org.scalatest._
import volang.ast._
import volang.lexer._

class LFSpec extends FlatSpec {
  // Make sure that this file is under LF
  "Lexer" should "handle LF files correctly" in {
    val input = """

      """
    val l     = new Lexer(input)
    assert(l.nextToken.isInstanceOf[LINEFEED])
    assert(l.nextToken.isInstanceOf[LINEFEED])
    assert(l.nextToken.isInstanceOf[EOF])
  }

}
