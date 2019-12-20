package volang.lexer

import org.scalatest._

class LFSpec extends FlatSpec {
  // Make sure that this file is under LF
  "Lexer" should "handle LF files correctly" in {
    val input = """
      """
    assert(new Lexer(input).nextToken.isInstanceOf[LINEFEED])
  }
}
