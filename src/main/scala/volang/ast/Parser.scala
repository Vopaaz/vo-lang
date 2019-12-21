package volang.ast

import volang.lexer._
import scala.reflect._

object Pri extends Enumeration {
  val lowest, ==, ><, +-, */, !, call = Value

  def priOf(token: TokenType) = {
    token match {
      case _: EQ  => Pri.==
      case _: NEQ => Pri.==

      case _: PLUS => Pri.+-
    }
  }
}

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
      case _: LINEFEED => {
        nextToken
        None
      }
      case other => Some(parseExpressionStatement)
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
    val expression = parseExpression(Pri.lowest)
    new LetStatement(identifier, expression)
  }

  private def parseReturnStatement: ReturnStatement = {
    expect[RETURN](nextToken)
    new ReturnStatement(parseExpression(Pri.lowest))
  }

  private def parseExpression(pri: Pri.Value): Expression = {
    val expression = peekToken match {
      case _: IDENTIFIER => parseIdentifier
      case _: NUMBER     => parseNumberLiteral
      case _: NOT        => parsePrefixExpression
      case _: MINUS      => parsePrefixExpression
      case others => {
        nextToken
        new Expression
      }
    }

    expression
  }

  private def parseExpressionStatement: ExpressionStatement = {
    new ExpressionStatement(parseExpression(Pri.lowest))
  }

  private def parseIdentifier: Identifier = {
    new Identifier(nextToken.asInstanceOf[IDENTIFIER])
  }

  private def parseNumberLiteral: NumberLiteral = {
    new NumberLiteral(nextToken.asInstanceOf[NUMBER])
  }

  private def parseBooleanLiteral: BooleanLiteral = {
    new BooleanLiteral(nextToken)
  }

  private def parsePrefixExpression: PrefixExpression = {
    val token = nextToken
    new PrefixExpression(
      token,
      parseExpression(
        token match {
          case _: MINUS => Pri.+-
          case _: NOT   => Pri.!
        }
      )
    )
  }

  def parse: Root = {
    val root = new Root(input)
    while (!peekToken.isInstanceOf[EOF]) {
      val s = parseStatement
      if (s.isDefined) {
        root.statements.append(s.get)
      }
    }
    root
  }
}
