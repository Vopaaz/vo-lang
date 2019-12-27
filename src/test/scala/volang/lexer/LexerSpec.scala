package volang.lexer

import org.scalatest.FlatSpec

class LexerSpec extends FlatSpec {

  def testInput(input: String, expected: List[TokenType]): Unit = {
    val l = new Lexer(input)
    expected.foreach(x => {
      val token = l.nextToken
      assert(x.getClass === token.getClass)
      assert(x.literal === token.literal)
    })
  }

  "Lexer" should "parse basic inputs" in {
    val input = "=+-()\n;"
    val expected = List[TokenType](
      new ASSIGN,
      new PLUS,
      new MINUS,
      new LPAREN,
      new RPAREN,
      new LINEFEED,
      new SEMICOLON,
      new EOF
    )

    testInput(input, expected)
  }

  it should "parse easy inputs" in {
    val input = """let x = 1
    let y = 2.5

    func add(a,b){
      return a+b
    }

    let result = add(x,y)
    """

    val expected = List[TokenType](
      new LET,
      new IDENTIFIER("x"),
      new ASSIGN,
      new NUMBER(1),
      new LINEFEED,
      new LET,
      new IDENTIFIER("y"),
      new ASSIGN,
      new NUMBER(2.5),
      new LINEFEED,
      new LINEFEED,
      new FUNC,
      new IDENTIFIER("add"),
      new LPAREN,
      new IDENTIFIER("a"),
      new COMMA,
      new IDENTIFIER("b"),
      new RPAREN,
      new LBRACE,
      new LINEFEED,
      new RETURN,
      new IDENTIFIER("a"),
      new PLUS,
      new IDENTIFIER("b"),
      new LINEFEED,
      new RBRACE,
      new LINEFEED,
      new LINEFEED,
      new LET,
      new IDENTIFIER("result"),
      new ASSIGN,
      new IDENTIFIER("add"),
      new LPAREN,
      new IDENTIFIER("x"),
      new COMMA,
      new IDENTIFIER("y"),
      new RPAREN,
      new LINEFEED,
      new EOF
    )

    testInput(input, expected)
  }

  it should "identify illegal tokens in some bad inputs" in {
    val l1 = new Lexer("func ?what {return 0}")
    l1.nextToken
    assert(l1.nextToken.getClass === (new ILLEGAL).getClass)

    val l2 = new Lexer("0.2.5")
    assert(l2.nextToken.getClass === (new ILLEGAL).getClass)
  }

  it should "parse complex inputs" in {
    val input = """x>=1.5
    y!=h
    c==a
    b+=c
    """
    val expected: List[TokenType] = List[TokenType](
      new IDENTIFIER("x"),
      new GEQ,
      new NUMBER(1.5),
      new LINEFEED,
      new IDENTIFIER("y"),
      new NEQ,
      new IDENTIFIER("h"),
      new LINEFEED,
      new IDENTIFIER("c"),
      new EQ,
      new IDENTIFIER("a"),
      new LINEFEED,
      new IDENTIFIER("b"),
      new PLUSEQ,
      new IDENTIFIER("c"),
      new LINEFEED,
      new EOF
    )
    testInput(input, expected)
  }

  it should "parse boolean correctly" in {
    val input = "x=true,y=false"
    val expected = List[TokenType](
      new IDENTIFIER("x"),
      new ASSIGN,
      new TRUE,
      new COMMA,
      new IDENTIFIER("y"),
      new ASSIGN,
      new FALSE,
      new EOF
    )
    testInput(input, expected)
  }

  it should "parse control flow" in {
    val input = "if( ifelse == 3){} else {while(1)}"
    val expected = List[TokenType](
      new IF,
      new LPAREN,
      new IDENTIFIER("ifelse"),
      new EQ,
      new NUMBER(3),
      new RPAREN,
      new LBRACE,
      new RBRACE,
      new ELSE,
      new LBRACE,
      new WHILE,
      new LPAREN,
      new NUMBER(1),
      new RPAREN,
      new RBRACE,
      new EOF
    )
    testInput(input, expected)
  }

  it should "parse string correctly" in {
    val input = """
    "This""Is"'Survival'
    "'"
    '"'
    """
    val expected = List[TokenType](
      new LINEFEED,
      new STRING("This"),
      new STRING("Is"),
      new STRING("Survival"),
      new LINEFEED,
      new STRING("'"),
      new LINEFEED,
      new STRING("\""),
      new LINEFEED,
      new EOF
    )
    testInput(input, expected)
  }

  it should "not confusing strings with others" in {
    val input = """
    "String" if true else 'another'
    """
    val expected = List[TokenType](
      new LINEFEED,
      new STRING("String"),
      new IF,
      new TRUE,
      new ELSE,
      new STRING("another"),
      new LINEFEED,
      new EOF
    )
    testInput(input, expected)
  }

  it should "handle incomplete strings" in {
    val input = """
    "should fail
    """
    val expected = List[TokenType](
      new LINEFEED,
      new ILLEGAL,
      new EOF
    )
    testInput(input, expected)
  }

  it should "parse '_' as valid identifier" in {
    val input = "_ __ __some__ that_is"
    val expected = List[TokenType](
      new IDENTIFIER("_"),
      new IDENTIFIER("__"),
      new IDENTIFIER("__some__"),
      new IDENTIFIER("that_is"),
    )
    testInput(input, expected)
  }

  it should "parse single number without linefeed" in {
    val input = "5.5"
    val expected = List[TokenType](
      new NUMBER(5.5)
    )
    testInput(input, expected)
  }
}
