package volang.eval

import volang.ast._
import volang.lexer._
import scala.util.control.Breaks._

object Evaluator {
  val env = new Environment

  def evaluate(node: Root): VoObject = {
    env.clear
    try {
      _evaluate(node)
    } catch {
      case x: UndefinedOperatorException => new VoError(x.message)
      case x: AssertionError             => new VoError(x.getMessage())
    }
  }

  private def _evaluate(node: Node): VoObject = {
    node match {
      case x: Root                => evalStatements(x.statements)
      case x: NumberLiteral       => new VoNumber(x.value)
      case x: BooleanLiteral      => new VoBoolean(x.value)
      case x: NoneLiteral         => new VoNone
      case x: PrefixExpression    => evalPrefixExpression(x)
      case x: InfixExpression     => evalInfixExpression(x)
      case x: IfExpression        => evalIfExpression(x)
      case x: BlockStatement      => evalBlockStatement(x)
      case x: Identifier          => evalIdentifier(x)
      case x: ExpressionStatement => _evaluate(x.expression)
      case x: LetStatement        => evalLetStatement(x)
    }
  }

  private def evalStatements(statements: List[Statement]): VoObject = {
    var obj: VoObject = new VoNone

    breakable {
      for (statement <- statements) {
        obj = _evaluate(statement)
        if (obj.isInstanceOf[VoError]) {
          break
        }
      }
    }

    obj
  }

  private def evalBlockStatement(statement: BlockStatement): VoObject = {
    evalStatements(statement.statements)
  }

  private def evalLetStatement(statement: LetStatement): VoObject = {
    val value = _evaluate(statement.expression)
    env.set(statement.identifier.value, value)
    value
  }

  private def evalPrefixExpression(expression: PrefixExpression): VoObject = {
    expression.operatorToken match {
      case _: NOT   => evalNotPrefixExpression(_evaluate(expression.right))
      case _: MINUS => evalMinusPrefixExpression(_evaluate(expression.right))
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
        _evaluate(expression.left) == _evaluate(expression.right)
      }
      case _: GEQ => {
        _evaluate(expression.left) >= _evaluate(expression.right)
      }
      case _: LEQ => {
        _evaluate(expression.left) <= _evaluate(expression.right)
      }
      case _: GT => {
        _evaluate(expression.left) > _evaluate(expression.right)
      }
      case _: LT => {
        _evaluate(expression.left) < _evaluate(expression.right)
      }
      case _: NEQ => {
        _evaluate(expression.left) != _evaluate(expression.right)
      }
      case _: PLUS => {
        _evaluate(expression.left) + _evaluate(expression.right)
      }
      case _: MINUS => {
        _evaluate(expression.left) - _evaluate(expression.right)
      }
      case _: TIMES => {
        _evaluate(expression.left) * _evaluate(expression.right)
      }
      case _: DIVIDE => {
        _evaluate(expression.left) / _evaluate(expression.right)
      }
    }
  }

  private def evalIfExpression(expression: IfExpression): VoObject = {
    if (_evaluate(expression.condition).asBoolean.value) {
      _evaluate(expression.thenBlock)
    } else {
      _evaluate(expression.elseBlock)
    }
  }

  private def evalIdentifier(identifier: Identifier): VoObject = {
    env.get(identifier.value)
  }
}
