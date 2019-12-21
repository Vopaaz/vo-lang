package volang.linefeed

import org.scalatest._
import volang.ast._
import volang.lexer._

class CRLFSpec extends FlatSpec {
  // Make sure that this file is under CRLF
  "Lexer" should "handle CRLF files correctly" in {
    val input = """

      """
    val l     = new Lexer(input)
    assert(l.nextToken.isInstanceOf[LINEFEED])
    assert(l.nextToken.isInstanceOf[LINEFEED])
    assert(l.nextToken.isInstanceOf[EOF])
  }

}
