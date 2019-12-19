package volang.ast
import volang.lexer._
import scala.collection.mutable._

abstract class Node

class Statement extends Node

class Expression extends Node

class SingleLiteralExpression(val token: TokenType) extends Expression {
  override def toString(): String = {
    token.literal.toString()
  }
}

class Root extends Node {
  val statements = new ListBuffer[Statement]
  override def toString(): String = {
    val buff = new StringBuilder
    statements.foreach(x => {
      buff.append(x.toString())
    })
    buff.append("\n")
    buff.mkString
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
