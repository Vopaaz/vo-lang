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

  "BlockStatement" should "print contents correctly" in {
    assert(
      new BlockStatement(
        List[Statement](
          new ExpressionStatement(new NumberLiteral(new NUMBER(1))),
          new LetStatement(
            new Identifier(new IDENTIFIER("x")),
            new BooleanLiteral(new TRUE)
          )
        )
      ).toString === """{
1.0
let x = true
}
"""
    )
  }

  it should "print empty block statements correctly" in {
    assert(new BlockStatement(List()).toString() === """{
}
""")
  }

  "IfExpression" should "print contents correctly" in {
    assert(
      new IfExpression(
        new BooleanLiteral(new TRUE),
        new BlockStatement(
          List[Statement](new ReturnStatement(new NumberLiteral(new NUMBER(2))))
        ),
        new BlockStatement(
          List[Statement](
            new ReturnStatement(new Identifier(new IDENTIFIER("x")))
          )
        )
      ).toString === """if true
then {
return 2.0
}
else {
return x
}
"""
    )
  }
}
