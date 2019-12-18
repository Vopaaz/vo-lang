package volang.lexer

abstract class TokenType {
  val literal: Any
  def cls = getClass()
  override def toString(): String = {
    s"Token ${cls.toString()} '$literal'"
  }
}

class ILLEGAL extends TokenType {
  val literal: String = "ILLEGAL"
}
class EOF extends TokenType {
  val literal: String = "EOF"
}

class IDENTIFIER(val literal: String) extends TokenType

class NUMBER(val literal: Double) extends TokenType
class STRING(val literal: String) extends TokenType

class ASSIGN extends TokenType {
  val literal: String = "="
}
class EQ extends TokenType {
  val literal: String = "=="
}
class PLUS extends TokenType {
  val literal: String = "+"
}
class PLUSEQ extends TokenType {
  val literal: String = "+="
}
class MINUS extends TokenType {
  val literal: String = "-"
}
class MINUSEQ extends TokenType {
  val literal: String = "-="
}
class TIMES extends TokenType {
  val literal: String = "*"
}
class TIMESEQ extends TokenType {
  val literal: String = "*="
}
class DIVIDE extends TokenType {
  val literal: String = "/"
}
class DIVIDEEQ extends TokenType {
  val literal: String = "/="
}
class NOT extends TokenType {
  val literal: String = "!"
}
class NEQ extends TokenType {
  val literal: String = "!="
}
class GT extends TokenType {
  val literal: String = ">"
}
class LT extends TokenType {
  val literal: String = "<"
}
class GEQ extends TokenType {
  val literal: String = ">="
}
class LEQ extends TokenType {
  val literal: String = "<="
}

class COMMA extends TokenType {
  val literal: String = ","
}
class DOT extends TokenType {
  val literal: String = "."
}
class LINEFEED extends TokenType {
  val literal: String = "LF"
}
class SEMICOLON extends TokenType {
  val literal: String = ";"
}
class BACKSLASH extends TokenType {
  val literal: String = "\\"
}

class LPAREN extends TokenType {
  val literal: String = "("
}
class RPAREN extends TokenType {
  val literal: String = ")"
}
class LBRACK extends TokenType {
  val literal: String = "["
}
class RBRACK extends TokenType {
  val literal: String = "]"
}
class LBRACE extends TokenType {
  val literal: String = "{"
}
class RBRACE extends TokenType {
  val literal: String = "}"
}

class FUNC extends TokenType {
  val literal: String = "func"
}
class LET extends TokenType {
  val literal: String = "let"
}
class RETURN extends TokenType {
  val literal: String = "return"
}
class IF extends TokenType {
  val literal: String = "if"
}
class ELSE extends TokenType {
  val literal: String = "else"
}
class WHILE extends TokenType {
  val literal: String = "while"
}
class FOR extends TokenType {
  val literal: String = "for"
}

class TRUE extends TokenType {
  val literal: Boolean = true
}
class FALSE extends TokenType {
  val literal: Boolean = false
}
class NONE extends TokenType{
  val literal: String = "none"
}
