package volang.ast

import volang.lexer._
import scala.reflect._
import scala.collection.mutable._

object Pri extends Enumeration {
  val lowest, ==, ><, +-, */, prefix, call = Value

  def of(token: TokenType) = {
    token match {
      case _: EQ     => Pri.==
      case _: NEQ    => Pri.==
      case _: LT     => Pri.><
      case _: GT     => Pri.><
      case _: LEQ    => Pri.><
      case _: GEQ    => Pri.><
      case _: PLUS   => Pri.+-
      case _: MINUS  => Pri.+-
      case _: TIMES  => Pri.*/
      case _: DIVIDE => Pri.*/
      case _: NOT    => Pri.prefix
      case _: LPAREN => Pri.call
      case others    => Pri.lowest
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
    var left: Option[Expression] = peekToken match {
      case _: NOT    => Some(parsePrefixExpression)
      case _: MINUS  => Some(parsePrefixExpression)
      case _: LPAREN => Some(parseGroupedExpression)
      case _: IF     => Some(parseIfExpression)
      case _: FUNC   => Some(parseFunctionExpression)
      case others    => None
    }

    if (!left.isDefined) {
      left = peekToken match {
        case _: IDENTIFIER => Some(parseIdentifier)
        case _: NUMBER     => Some(parseNumberLiteral)
        case _: TRUE       => Some(parseBooleanLiteral)
        case _: FALSE      => Some(parseBooleanLiteral)
      }
    }

    while (!peekToken.isInstanceOf[LINEFEED] && !peekToken
             .isInstanceOf[EOF] && pri < Pri.of(peekToken)) {
      left = peekToken match {
        case _: LPAREN => Some(parseCallExpression(left.get))
        case other     => Some(parseInfixExpression(left.get))
      }
    }

    if (left.isDefined) {
      left.get
    } else {
      throw new ParsingException("Parsing error, no expression defined.")
    }
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
      parseExpression(Pri.prefix)
    )
  }

  private def parseInfixExpression(left: Expression): InfixExpression = {
    val token = nextToken
    new InfixExpression(left, token, parseExpression(Pri.of(token)))
  }

  def parse: Root = {
    val root = new Root(input)
    while (!peekToken.isInstanceOf[EOF]) {
      val s = parseStatement
      if (s.isDefined) {
        root.append(s.get)
      }
    }
    root
  }

  private def parseGroupedExpression: Expression = {
    expect[LPAREN](nextToken)
    val exp = parseExpression(Pri.lowest)
    expect[RPAREN](nextToken)
    exp
  }

  private def parseIfExpression: IfExpression = {
    expect[IF](nextToken)
    expect[LPAREN](nextToken)
    val condition = parseExpression(Pri.lowest)
    expect[RPAREN](nextToken)
    val thenBlock = parseBlockStatement

    if (peekToken.isInstanceOf[ELSE]) {
      nextToken
      val elseBlock = parseBlockStatement
      new IfExpression(condition, thenBlock, elseBlock)
    } else {
      new IfExpression(condition, thenBlock)
    }
  }

  private def parseBlockStatement: BlockStatement = {
    val buff = new ListBuffer[Statement]
    expect[LBRACE](nextToken)
    while (!peekToken.isInstanceOf[RBRACE] && !peekToken.isInstanceOf[EOF]) {
      val stmt = parseStatement
      if (stmt.isDefined) {
        buff.append(stmt.get)
      }
    }
    expect[RBRACE](nextToken)
    new BlockStatement(buff.toList)
  }

  private def parseFunctionExpression: FunctionLiteral = {
    expect[FUNC](nextToken)
    new FunctionLiteral(parseFunctionParameters, parseBlockStatement)
  }

  private def parseFunctionParameters: List[Identifier] = {
    val buff = new ListBuffer[Identifier]
    expect[LPAREN](nextToken)

    while (!peekToken.isInstanceOf[RPAREN] && !peekToken.isInstanceOf[EOF]) {
      expect[IDENTIFIER](peekToken)
      buff.append(new Identifier(nextToken.asInstanceOf[IDENTIFIER]))
      if (peekToken.isInstanceOf[COMMA]) {
        nextToken
      }
    }

    expect[RPAREN](nextToken)
    buff.toList
  }

  private def parseCallExpression(function: Expression): CallExpression = {
    new CallExpression(function, parseCallArguments)
  }

  private def parseCallArguments: List[Expression] = {
    val buff = new ListBuffer[Expression]
    expect[LPAREN](nextToken)
    if (!peekToken.isInstanceOf[RPAREN]) {
      buff.append(parseExpression(Pri.lowest))
      while (peekToken.isInstanceOf[COMMA]) {
        nextToken
        buff.append(parseExpression(Pri.lowest))
      }
    }
    expect[RPAREN](nextToken)
    buff.toList
  }
}
