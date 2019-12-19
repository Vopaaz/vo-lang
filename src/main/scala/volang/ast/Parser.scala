package volang.ast

import volang.lexer._

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
      case let: LET => Some(parseLetStatement)
      case other => {
        nextToken
        None
      }
    }
  }

  private def parseLetStatement: LetStatement = {
    if (!nextToken.isInstanceOf[LET]) {
      throw new ParsingException(s"A 'let' is expected, $lastToken found.")
    }

    if (!peekToken.isInstanceOf[IDENTIFIER]) {
      throw new ParsingException(
        s"An identifier is expected, $peekToken found."
      )
    }
    val identifier = nextToken.asInstanceOf[IDENTIFIER]

    if (!nextToken.isInstanceOf[ASSIGN]) {
      throw new ParsingException(
        s"A '=' is expected, $lastToken found."
      )
    }

    val expression = parseExpression
    new LetStatement(identifier, expression)
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
