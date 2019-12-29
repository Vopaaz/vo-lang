package volang.eval
import volang.ast._
import volang.lexer._

object Evaluator {
  def evaluate(node: Node): VoObject = {
    node match {
      case root: Root                => evalStatements(root.statements)
      case stmt: Statement           => evalStatement(stmt)
      case number: NumberLiteral     => new VoNumber(number.value)
      case bool: BooleanLiteral      => new VoBoolean(bool.value)
      case none: NoneLiteral         => new VoNone
      case prefExp: PrefixExpression => evalPrefixExpression(prefExp)
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
    }
  }
}
