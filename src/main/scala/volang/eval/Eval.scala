package volang.eval
import volang.ast._
import volang.lexer._

object Evaluator {
  def evaluate(node: Node): VoObject = {
    node match {
      case x: Root             => evalStatements(x.statements)
      case x: Statement        => evalStatement(x)
      case x: NumberLiteral    => new VoNumber(x.value)
      case x: BooleanLiteral   => new VoBoolean(x.value)
      case x: NoneLiteral      => new VoNone
      case x: PrefixExpression => evalPrefixExpression(x)
      case x: InfixExpression  => evalInfixExpression(x)
    }
  }

  private def evalStatements(statements: List[Statement]): VoObject = {
    statements.map(x => evalStatement(x)).lastOption.getOrElse(new VoNone)
  }

  private def evalStatement(statement: Statement): VoObject = {
    statement match {
      case expStmt: ExpressionStatement => evaluate(expStmt.expression)
    }
  }

  private def evalPrefixExpression(expression: PrefixExpression): VoObject = {
    expression.operatorToken match {
      case _: NOT   => evalNotPrefixExpression(evaluate(expression.right))
      case _: MINUS => evalMinusPrefixExpression(evaluate(expression.right))
    }
  }

  private def evalNotPrefixExpression(right: VoObject): VoObject = {
    right match {
      case bool: VoBoolean => new VoBoolean(!bool.value)
      case num: VoNumber   => new VoBoolean(num.value == 0)
    }
  }

  private def evalMinusPrefixExpression(right: VoObject): VoObject = {
    right match {
      case num: VoNumber => new VoNumber(-num.value)
    }
  }

  private def evalInfixExpression(expression: InfixExpression): VoObject = {
    expression.operatorToken match {
      case _: EQ =>
        new VoBoolean(
          evaluate(expression.left).value == evaluate(expression.right).value
        )
      case _: GEQ => {
        new VoBoolean(evaluate(expression.left) >= evaluate(expression.right))
      }
      case _: LEQ => {
        new VoBoolean(evaluate(expression.left) <= evaluate(expression.right))
      }
      case _: GT => {
        new VoBoolean(evaluate(expression.left) > evaluate(expression.right))
      }
      case _: LT => {
        new VoBoolean(evaluate(expression.left) < evaluate(expression.right))
      }
      case _: NEQ => {
        new VoBoolean(evaluate(expression.left) != evaluate(expression.right))
      }
    }
  }
}
