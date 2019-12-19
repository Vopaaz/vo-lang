package volang.lexer

import scala.collection.mutable.ArrayBuffer

class Lexer(input: String) {
  private var position: Int   = 0
  private val NULL_CHAR: Char = 0.toChar

  def hasNext: Boolean = {
    position <= input.length
  }

  private def peekChar: Char = {
    var c: Char = NULL_CHAR
    if (position < input.length()) {
      c = input.charAt(position)
    } else {
      c = NULL_CHAR
    }
    c
  }

  private def nextChar: Char = {
    val c = peekChar
    position += 1
    c
  }

  private def isLetterOrUnderscore(c: Char): Boolean = {
    c.isLetter || c == '_'
  }

  private def isDigitOrDot(c: Char): Boolean = {
    c.isDigit || c == '.'
  }

  private def readNumber(head: Char): TokenType = {
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

  private def readIdentifierOrKeyword(head: Char): TokenType = {
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
      case "none"   => new NONE
      case other    => new IDENTIFIER(other)
    }
  }

  private def readWord(head: Char): TokenType = {
    if (head.isDigit) {
      readNumber(head)
    } else if (isLetterOrUnderscore(head)) {
      readIdentifierOrKeyword(head)
    } else {
      new ILLEGAL
    }
  }

  private def skipSpace: Unit = {
    while (peekChar == '\t' || peekChar == ' ') {
      position += 1
    }
  }

  private def createTokenOnFollowedByEQ(
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

  private def startsWithEqual: TokenType =
    createTokenOnFollowedByEQ(new EQ, new ASSIGN)

  private def startsWithPlus: TokenType =
    createTokenOnFollowedByEQ(new PLUSEQ, new PLUS)

  private def startsWithMinus: TokenType =
    createTokenOnFollowedByEQ(new MINUSEQ, new MINUS)

  private def startsWithTimes: TokenType =
    createTokenOnFollowedByEQ(new TIMESEQ, new TIMES)

  private def startsWithDivide: TokenType =
    createTokenOnFollowedByEQ(new DIVIDEEQ, new DIVIDE)

  private def startsWithNot: TokenType =
    createTokenOnFollowedByEQ(new NEQ, new NOT)

  private def startsWithGT: TokenType =
    createTokenOnFollowedByEQ(new GEQ, new GT)

  private def startsWithLT: TokenType =
    createTokenOnFollowedByEQ(new LEQ, new LT)

  private def catchString(c: Char): TokenType = {
    val buff = new StringBuilder
    while (hasNext && peekChar != c) {
      buff.append(nextChar)
    }
    if (hasNext) {
      nextChar
      new STRING(buff.mkString)
    } else {
      new ILLEGAL
    }
  }

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
      case '\"'        => catchString('\"')
      case '\''        => catchString('\'')
      case other       => readWord(other)
    }
  }
}
