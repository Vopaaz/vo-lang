package volang.ast

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {

  "Parser" should "parse simple let statements" in {
    val input = """
        let x = 10
        let y=x
        let z = 10 + 10 *100
        """

    val p: Parser  = new Parser(input)
    val statements = p.parse.statements
    assert(statements.length === 3)
    val s1_raw = statements(0)
    assert(s1_raw.isInstanceOf[LetStatement])
    val s1 = s1_raw.asInstanceOf[LetStatement]
    val s2 = statements(1).asInstanceOf[LetStatement]
    val s3 = statements(2).asInstanceOf[LetStatement]
    assert(s1.identifier.literal === "x")
    assert(s2.identifier.literal === "y")
    assert(s3.identifier.literal === "z")
  }

  it should "fail in invalid let statements" in {
    val input = "let = 1"
    a[ParsingException] should be thrownBy {
      new Parser(input).parse
    }
  }
}
