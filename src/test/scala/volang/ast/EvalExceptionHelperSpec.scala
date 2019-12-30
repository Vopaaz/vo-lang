package volang.ast

import org.scalatest._
import volang.ast.Pri
import volang.lexer._

class EvalExceptionHelperSpec extends FlatSpec with Matchers {
  "Parser" should "parse '1>2 != 2>3'" in {
    val s = new Parser("1>2 != 2>3").parse.statements.head.toString() === "((1 > 2) != (2 > 3))"
  }
}
