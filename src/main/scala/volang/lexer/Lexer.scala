package volang.lexer

import scala.collection.mutable.ArrayBuffer

class Lexer(val input: String) {
  var position: Int   = 0
  val NULL_CHAR: Char = 0.toChar

  def hasNext: Boolean = {
    position <= input.length
  }

  def peekChar: Char = {
    var c: Char = NULL_CHAR
    if (position < input.length()) {
      c = input.charAt(position)
    } else {
      c = NULL_CHAR
    }
    c
  }

  def nextChar: Char = {
    val c = peekChar
    position += 1
    c
  }

  def isLetterOrUnderscore(c: Char): Boolean = {
    c.isLetter || c == '_'
  }

  def isDigitOrDot(c: Char): Boolean = {
    c.isDigit || c == '.'
  }

  def readNumber(head: Char): TokenType = {
    val buff = new StringBuilder
    buff.append(head)
    while (isDigitOrDot(peekChar)) {
      buff.append(nextChar)
    }
    val s = buff.mkString
    if (s.count(x => x == '.') > 1) {
      new ILLEGAL
    } else {
      new NUMBER(s.toDouble)
    }
  }

  def readIdentifierOrKeyword(head: Char): TokenType = {
    val buff = new StringBuilder
    buff.append(head)
    while (isLetterOrUnderscore(peekChar) || peekChar.isDigit) {
      buff.append(nextChar)
    }
    val s = buff.mkString
    s match {
      case "func"   => new FUNC
      case "let"    => new LET
      case "return" => new RETURN
      case "if"     => new IF
      case "else"   => new ELSE
      case "for"    => new FOR
      case "while"  => new WHILE
      case "true"   => new TRUE
      case "false"  => new FALSE
      case other    => new IDENTIFIER(other)
    }
  }

  def readWord(head: Char): TokenType = {
    if (head.isDigit) {
      readNumber(head)
    } else if (isLetterOrUnderscore(head)) {
      readIdentifierOrKeyword(head)
    } else {
      new ILLEGAL
    }
  }

  def skipSpace: Unit = {
    while (peekChar == '\t' || peekChar == ' ') {
      position += 1
    }
  }

  def createTokenOnFollowedByEQ(
      followed: TokenType,
      notFollowed: TokenType
  ): TokenType = {
    if (peekChar == '=') {
      nextChar
      followed
    } else {
      notFollowed
    }
  }

  def startsWithEqual: TokenType = createTokenOnFollowedByEQ(new EQ, new ASSIGN)

  def startsWithPlus: TokenType =
    createTokenOnFollowedByEQ(new PLUSEQ, new PLUS)

  def startsWithMinus: TokenType =
    createTokenOnFollowedByEQ(new MINUSEQ, new MINUS)

  def startsWithTimes: TokenType =
    createTokenOnFollowedByEQ(new TIMESEQ, new TIMES)

  def startsWithDivide: TokenType =
    createTokenOnFollowedByEQ(new DIVIDEEQ, new DIVIDE)

  def startsWithNot: TokenType = createTokenOnFollowedByEQ(new NEQ, new NOT)

  def startsWithGT: TokenType = createTokenOnFollowedByEQ(new GEQ, new GT)

  def startsWithLT: TokenType = createTokenOnFollowedByEQ(new LEQ, new LT)

  def nextToken: TokenType = {
    skipSpace
    nextChar match {
      case '='         => startsWithEqual
      case '+'         => startsWithPlus
      case '-'         => startsWithMinus
      case '*'         => startsWithTimes
      case '/'         => startsWithDivide
      case '!'         => startsWithNot
      case '>'         => startsWithGT
      case '<'         => startsWithLT
      case ','         => new COMMA
      case '.'         => new DOT
      case '\n'        => new LINEFEED
      case ';'         => new SEMICOLON
      case '('         => new LPAREN
      case ')'         => new RPAREN
      case '['         => new LBRACK
      case ']'         => new RBRACK
      case '{'         => new LBRACE
      case '}'         => new RBRACE
      case '\\'        => new BACKSLASH
      case `NULL_CHAR` => new EOF
      case other       => readWord(other)
    }
  }
}
