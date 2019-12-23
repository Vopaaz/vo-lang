package volang.ast

import org.scalatest._
import volang.lexer._

class NodeSpec extends FlatSpec {
  "LetStatement" should "be able to output test strings" in {
    assert(
      new LetStatement(
        new Identifier(new IDENTIFIER("a")),
        new SingleLiteralExpression(new IDENTIFIER("b"))
      ).toString() === "let a = b\n"
    )
  }

  "PrefixExpression" should "print results correctly" in {
    val exp = new PrefixExpression(new MINUS, new NumberLiteral(new NUMBER(1)))
    assert(exp.toString() === "(-1.0)")
  }

  "InfixExpression" should "print simple results correctly" in {
    val exp = new InfixExpression(
      new NumberLiteral(new NUMBER(1)),
      new PLUS,
      new NumberLiteral(new NUMBER(2))
    )
    assert(exp.toString() === "(1.0 + 2.0)")
  }

  it should "print complex results correctly" in {
    val expInner = new InfixExpression(
      new NumberLiteral(new NUMBER(1)),
      new PLUS,
      new NumberLiteral(new NUMBER(2))
    )
    val exp = new InfixExpression(
      expInner,
      new MINUS,
      expInner
    )
    assert(exp.toString() === "((1.0 + 2.0) - (1.0 + 2.0))")
  }
}
