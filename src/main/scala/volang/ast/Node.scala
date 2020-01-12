package volang.ast
import volang.lexer._
import scala.collection.mutable._

abstract class Node

abstract class Statement extends Node

abstract class Expression extends Node

class SingleLiteralExpression(val token: TokenType) extends Expression {
  override def toString(): String = {
    token.literal.toString()
  }
}

class Root(val src: String) extends Node {
  private val _statements = new ListBuffer[Statement]
  override def toString(): String = {
    val buff = new StringBuilder(s"---Source---\n$src\n---Statements---\n")
    statements.foreach(x => {
      buff.append(x.toString())
    })
    buff.append("\n---------\n")
    buff.mkString
  }

  def append(statement: Statement) = {
    _statements.append(statement)
  }

  def statements: List[Statement] = {
    _statements.toList
  }
}

class LetStatement(val identifier: Identifier, val expression: Expression)
    extends Statement {
  override def toString(): String = {
    "let " + identifier.toString + " = " + expression.toString() + "\n"
  }
}

class ReturnStatement(val expression: Expression) extends Statement {
  override def toString(): String = {
    "return " + expression.toString() + "\n"
  }
}

class ExpressionStatement(val expression: Expression) extends Statement {
  override def toString(): String = {
    expression.toString() + "\n"
  }
}

class Identifier(val token: IDENTIFIER) extends Expression {
  val value = token.literal
  override def toString(): String = {
    value
  }
}

class NumberLiteral(val token: NUMBER) extends Expression {
  val value = token.literal
  override def toString(): String = {
    value.toString
  }
}

class BooleanLiteral(val token: TokenType) extends Expression {
  val value = token match {
    case x: TRUE  => x.literal
    case x: FALSE => x.literal
  }
  override def toString(): String = {
    value.toString
  }
}

class NoneLiteral(val token: NONE) extends Expression {
  val value = None
  override def toString(): String = {
    "none"
  }
}

class PrefixExpression(val operatorToken: TokenType, val right: Expression)
    extends Expression {
  override def toString(): String = {
    s"(${operatorToken.literal}${right.toString()})"
  }
}

class InfixExpression(
    val left: Expression,
    val operatorToken: TokenType,
    val right: Expression
) extends Expression {
  override def toString(): String = {
    s"(${left.toString} ${operatorToken.literal.toString} ${right.toString})"
  }
}

class BlockStatement(val statements: List[Statement]) extends Statement {
  override def toString(): String = {
    "{\n" +
      statements
        .map(x => x.toString)
        .mkString +
      "}\n"
  }
}

class EmptyBlockStatement extends BlockStatement(List())

class IfExpression(
    val condition: Expression,
    val thenBlock: BlockStatement,
    val elseBlock: BlockStatement = new EmptyBlockStatement
) extends Expression {
  override def toString(): String = {
    "if " + condition.toString() + "\nthen " + thenBlock
      .toString() + "else " + elseBlock.toString()
  }
}

class FunctionLiteral(
    val parameters: List[Identifier],
    val block: BlockStatement
) extends Expression {
  override def toString(): String = {
    "func(" +
      parameters
        .map(x => x.value)
        .mkString(", ") +
      ")" + block.toString()
  }
}

class CallExpression(val function: Expression, val arguments: List[Expression])
    extends Expression {
  assert(
    function.isInstanceOf[FunctionLiteral] || function.isInstanceOf[Identifier]
  )
  override def toString(): String = {
    function
      .toString() + "(" + arguments.map(x => x.toString()).mkString(", ") + ")"
  }
}

class StringLiteral(val token: STRING) extends Expression {
  val value = token.literal
  override def toString(): String = {
    value
  }
}

class DictLiteral(val pairs: List[Tuple2[Expression, Expression]])
    extends Expression {
  override def toString(): String = {
    "dict {" + pairs
      .map(x => {
        x._1.toString() + " -> " + x._2.toString()
      })
      .mkString(", ") + "}"
  }
}
