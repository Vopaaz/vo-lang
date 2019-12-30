package volang.ast

import org.scalatest._
import volang.ast.Pri
import volang.lexer._

class EvalExceptionHelperSpec extends FlatSpec with Matchers {
  "Parser" should "parse some expressions" in {
    assert(
      new Parser("1>2 != 2>3").parse.statements.head
        .toString().trim === "((1.0 > 2.0) != (2.0 > 3.0))"
    )
    assert(
      new Parser("(1+(-2))/10").parse.statements.head
        .toString().trim === "((1.0 + (-2.0)) / 10.0)"
    )
    assert(
      new Parser("(1+-1)/10").parse.statements.head
        .toString().trim === "((1.0 + (-1.0)) / 10.0)"
    )
  }
}
