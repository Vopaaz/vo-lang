package volang.ast

import volang.lexer._
import scala.reflect._

class Parser(input: String) {
  private val l: Lexer             = new Lexer(input)
  private var peekToken: TokenType = l.nextToken
  private var lastToken: TokenType = new ILLEGAL

  private def nextToken: TokenType = {
    val token = peekToken
    lastToken = token
    peekToken = l.nextToken
    token
  }

  private def parseStatement: Option[Statement] = {
    peekToken match {
      case _: LET    => Some(parseLetStatement)
      case _: RETURN => Some(parseReturnStatement)
      case other => {
        nextToken
        None
      }
    }
  }

  private def expect[T: ClassTag](token: TokenType) = {
    if (!classTag[T].runtimeClass.isInstance(token)) {
      throw new ParsingException(
        s"A '${classTag[T].toString}' is expected, $lastToken found."
      )
    }
  }

  private def parseLetStatement: LetStatement = {
    expect[LET](nextToken)
    expect[IDENTIFIER](peekToken)
    val identifier = new Identifier(nextToken.asInstanceOf[IDENTIFIER])
    expect[ASSIGN](nextToken)
    val expression = parseExpression
    new LetStatement(identifier, expression)
  }

  private def parseReturnStatement: ReturnStatement = {
    expect[RETURN](nextToken)
    new ReturnStatement(parseExpression)
  }

  private def parseExpression: Expression = {
    new Expression
  }

  def parse: Root = {
    val root = new Root
    while (!peekToken.isInstanceOf[EOF]) {
      val s = parseStatement
      if (s.isDefined) {
        root.statements.append(s.get)
      }
    }
    root
  }
}
