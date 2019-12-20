package volang.lexer

import org.scalatest._

class CRLFSpec extends FlatSpec {
  // Make sure that this file is under CRLF
  "Lexer" should "handle CRLF files correctly" in {
    val input = """
      """
    assert(new Lexer(input).nextToken.isInstanceOf[LINEFEED])
  }
}
