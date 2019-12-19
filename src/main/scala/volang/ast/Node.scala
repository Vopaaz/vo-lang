package volang.ast
import volang.lexer.IDENTIFIER
import scala.collection.mutable._

abstract class Node

class Statement extends Node

class Expression extends Node

class Root extends Node {
  val statements = new ListBuffer[Statement]
}

class LetStatement(val identifier: IDENTIFIER, val expression: Expression)
    extends Statement

class ReturnStatement(val expression: Expression) extends Statement
