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
    assert(s1.identifier.value === "x")
    assert(s2.identifier.value === "y")
    assert(s3.identifier.value === "z")
  }

  it should "fail in invalid let statements" in {
    a[ParsingException] should be thrownBy {
      new Parser("let = 1").parse
    }

    a[ParsingException] should be thrownBy {
      new Parser("let x 1").parse
    }

    a[ParsingException] should be thrownBy {
      new Parser("let 1 = x").parse
    }

    a[ParsingException] should be thrownBy {
      new Parser("let x").parse
    }
  }

  it should "parse simple return statements" in {
    val input = """
        return 3+5
        return 1
        return 12
        """

    val p: Parser  = new Parser(input)
    val statements = p.parse.statements
    assert(statements.length === 3)
    val s1_raw = statements(0)
    assert(s1_raw.isInstanceOf[ReturnStatement])
    val s1 = s1_raw.asInstanceOf[ReturnStatement]
    val s2 = statements(1).asInstanceOf[ReturnStatement]
    val s3 = statements(2).asInstanceOf[ReturnStatement]
  }

  it should "parse single expression statements" in {
    val statements = new Parser("""s
    """).parse.statements
    assert(statements.length === 1)
    val statementRaw = statements(0)
    assert(statementRaw.isInstanceOf[ExpressionStatement])
    val statement = statementRaw.asInstanceOf[ExpressionStatement]
  }
}
