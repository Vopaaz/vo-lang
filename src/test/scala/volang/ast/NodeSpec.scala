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
}
