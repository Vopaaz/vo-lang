package volang.eval

import volang.ast._
import volang.lexer._
import scala.util.control.Breaks._

object Evaluator {
  val env = new Environment

  def evaluate(node: Root): VoObject = {
    env.clear
    evaluateContinuous(node)
  }

  def evaluateContinuous(node: Root): VoObject = {
    try {
      _evaluate(node)
    } catch {
      case x: UndefinedOperatorException => new VoError(x.message)
      case x: AssertionError             => new VoError(x.getMessage())
    }
  }

  private def _evaluate(
      node: Node,
      environment: Environment = env
  ): VoObject = {
    node match {
      case x: Root                => evalStatements(x.statements, environment)
      case x: NumberLiteral       => new VoNumber(x.value)
      case x: BooleanLiteral      => new VoBoolean(x.value)
      case x: NoneLiteral         => new VoNone
      case x: PrefixExpression    => evalPrefixExpression(x, environment)
      case x: InfixExpression     => evalInfixExpression(x, environment)
      case x: IfExpression        => evalIfExpression(x, environment)
      case x: BlockStatement      => evalBlockStatement(x, environment)
      case x: Identifier          => evalIdentifier(x, environment)
      case x: ExpressionStatement => _evaluate(x.expression, environment)
      case x: LetStatement        => evalLetStatement(x, environment)
      case x: FunctionLiteral     => evalFunctionLiteral(x)
      case x: CallExpression      => evalCallExpression(x, environment)
    }
  }

  private def evalStatements(
      statements: List[Statement],
      environment: Environment = env
  ): VoObject = {
    var obj: VoObject = new VoNone

    breakable {
      for (statement <- statements) {
        obj = _evaluate(statement, environment)
        if (obj.isInstanceOf[VoError]) {
          break
        }
      }
    }

    obj
  }

  private def evalBlockStatement(
      statement: BlockStatement,
      environment: Environment = env
  ): VoObject = {
    evalStatements(statement.statements, environment)
  }

  private def evalLetStatement(
      statement: LetStatement,
      environment: Environment = env
  ): VoObject = {
    val value = _evaluate(statement.expression)
    environment.set(statement.identifier.value, value)
    value
  }

  private def evalPrefixExpression(
      expression: PrefixExpression,
      environment: Environment
  ): VoObject = {
    expression.operatorToken match {
      case _: NOT =>
        evalNotPrefixExpression(_evaluate(expression.right, environment))
      case _: MINUS =>
        evalMinusPrefixExpression(_evaluate(expression.right, environment))
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

  private def evalInfixExpression(
      expression: InfixExpression,
      environment: Environment = env
  ): VoObject = {
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

  private def evalIfExpression(
      expression: IfExpression,
      environment: Environment = env
  ): VoObject = {
    if (_evaluate(expression.condition).asBoolean.value) {
      _evaluate(expression.thenBlock)
    } else {
      _evaluate(expression.elseBlock)
    }
  }

  private def evalIdentifier(
      identifier: Identifier,
      environment: Environment = env
  ): VoObject = {
    environment.get(identifier.value)
  }

  private def evalFunctionLiteral(function: FunctionLiteral): VoFunction = {
    new VoFunction(function.parameters, function.block)
  }

  private def evalCallExpression(
      expression: CallExpression,
      environment: Environment = env
  ): VoObject = {
    val funcRaw = _evaluate(expression.function, environment)
    assert(funcRaw.isInstanceOf[VoFunction])
    val func    = funcRaw.asInstanceOf[VoFunction]
    val funcEnv = new Environment
    func.parameters
      .zip(expression.arguments)
      .foreach(
        x => {
          funcEnv.set(x._1.value, _evaluate(x._2, environment))
        }
      )
    _evaluate(func.block, funcEnv)
  }
}
