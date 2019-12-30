package volang.eval

import volang.ast._
import volang.lexer._
import scala.util.control.Breaks._

object Evaluator {
  def evaluate(node: Node): VoObject = {
    try {
      _evaluate(node)
    } catch {
      case x: UndefinedOperatorException => new VoError(x.message)
      case x: AssertionError             => new VoError(x.getMessage())
    }
  }

  def _evaluate(node: Node): VoObject = {
    node match {
      case x: Root             => evalStatements(x.statements)
      case x: NumberLiteral    => new VoNumber(x.value)
      case x: BooleanLiteral   => new VoBoolean(x.value)
      case x: NoneLiteral      => new VoNone
      case x: PrefixExpression => evalPrefixExpression(x)
      case x: InfixExpression  => evalInfixExpression(x)
      case x: IfExpression     => evalIfExpression(x)
      case x: BlockStatement   => evalBlockStatement(x)
    }
  }

  private def evalStatements(statements: List[Statement]): VoObject = {
    var obj: VoObject = new VoNone

    breakable {
      for (statement <- statements) {
        obj = evalStatement(statement)
        if (obj.isInstanceOf[VoError]) {
          break
        }
      }
    }

    obj
  }

  private def evalStatement(statement: Statement): VoObject = {
    statement match {
      case expStmt: ExpressionStatement => evaluate(expStmt.expression)
    }
  }

  private def evalBlockStatement(statement: BlockStatement): VoObject = {
    evalStatements(statement.statements)
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
      case _: EQ => {
        evaluate(expression.left) == evaluate(expression.right)
      }
      case _: GEQ => {
        evaluate(expression.left) >= evaluate(expression.right)
      }
      case _: LEQ => {
        evaluate(expression.left) <= evaluate(expression.right)
      }
      case _: GT => {
        evaluate(expression.left) > evaluate(expression.right)
      }
      case _: LT => {
        evaluate(expression.left) < evaluate(expression.right)
      }
      case _: NEQ => {
        evaluate(expression.left) != evaluate(expression.right)
      }
      case _: PLUS => {
        evaluate(expression.left) + evaluate(expression.right)
      }
      case _: MINUS => {
        evaluate(expression.left) - evaluate(expression.right)
      }
      case _: TIMES => {
        evaluate(expression.left) * evaluate(expression.right)
      }
      case _: DIVIDE => {
        evaluate(expression.left) / evaluate(expression.right)
      }
    }
  }

  private def evalIfExpression(expression: IfExpression): VoObject = {
    if (evaluate(expression.condition).asBoolean.value) {
      evaluate(expression.thenBlock)
    } else {
      evaluate(expression.elseBlock)
    }
  }
}
