package volang.ast

import org.scalatest._
import volang.ast.Pri
import volang.lexer._

class ParserSpec extends FlatSpec with Matchers {
  "Priorities" should "be organized well" in {
    assert(Pri.lowest < Pri.><)
    assert(Pri.+- < Pri.*/)
    assert(Pri.*/ < Pri.call)
  }

  "Parser" should "parse simple let statements" in {
    val input = """
        let x = 10
        let y=x
        let z = true
        """

    val p: Parser  = new Parser(input)
    val statements = p.parse.statements
    assert(statements.length === 3)
    val s1_raw = statements(0)
    assert(s1_raw.isInstanceOf[LetStatement])
    val s1 = s1_raw.asInstanceOf[LetStatement]
    val s2 = statements(1).asInstanceOf[LetStatement]
    val s3 = statements(2).asInstanceOf[LetStatement]
    assert(s1.identifier.value === "x")
    assert(s2.identifier.value === "y")
    assert(s3.identifier.value === "z")
  }

  it should "fail in invalid let statements" in {
    a[ParsingException] should be thrownBy {
      new Parser("let = 1").parse
    }

    a[ParsingException] should be thrownBy {
      new Parser("let x 1").parse
    }

    a[ParsingException] should be thrownBy {
      new Parser("let 1 = x").parse
    }

    a[ParsingException] should be thrownBy {
      new Parser("let x").parse
    }
  }

  it should "handle empty lines" in {
    val input = """



    """
    assert(new Parser(input).parse.statements.length === 0)
  }

  it should "parse simple return statements returning number" in {
    val input = """
        return 3
        return 1
        return 12
        """

    val p: Parser  = new Parser(input)
    val statements = p.parse.statements
    assert(statements.length === 3)
    val s1_raw = statements(0)
    assert(s1_raw.isInstanceOf[ReturnStatement])
    val s1 = s1_raw.asInstanceOf[ReturnStatement]
    val s2 = statements(1).asInstanceOf[ReturnStatement]
    val s3 = statements(2).asInstanceOf[ReturnStatement]
  }

  it should "parse single identifier expression statement" in {
    def checkStatement(statementRaw: Statement) = {
      assert(statementRaw.isInstanceOf[ExpressionStatement])
      val statement  = statementRaw.asInstanceOf[ExpressionStatement]
      val expression = statement.expression
      assert(expression.isInstanceOf[Identifier])
      assert(expression.asInstanceOf[Identifier].value === "s")
    }
    val statements = new Parser("""s
    s
    s
    """).parse.statements
    assert(statements.length === 3)
    statements.foreach(checkStatement)
  }

  it should "parse single number expression statement" in {
    val statements = new Parser("5.5").parse.statements
    assert(statements.length === 1)
    val statementRaw = statements(0)
    assert(statementRaw.isInstanceOf[ExpressionStatement])
    val expressionRaw =
      statementRaw.asInstanceOf[ExpressionStatement].expression
    assert(expressionRaw.isInstanceOf[NumberLiteral])
    val expression = expressionRaw.asInstanceOf[NumberLiteral]
    assert(expression.value === 5.5)
    assert(expression.toString === "5.5")
  }

  it should "parse prefix expression, decorating numbers, statement" in {
    val input = """
    -5
    !10
    -0.2
    """

    val statements = new Parser(input).parse.statements
    assert(statements.length == 3)
    val zipped = statements.zip(
      List[List[TokenType]](
        List[TokenType](new MINUS, new NUMBER(5)),
        List[TokenType](new NOT, new NUMBER(10)),
        List[TokenType](new MINUS, new NUMBER(0.2))
      )
    )
    for ((statementRaw: Statement, expected: List[TokenType]) <- zipped) {
      assert(statementRaw.isInstanceOf[ExpressionStatement])
      val expressionRaw =
        statementRaw.asInstanceOf[ExpressionStatement].expression
      assert(expressionRaw.isInstanceOf[PrefixExpression])
      val expression = expressionRaw.asInstanceOf[PrefixExpression]
      assert(expression.operatorToken.getClass === expected(0).getClass)
      assert(
        expression.right.asInstanceOf[NumberLiteral].token.literal === expected(
          1
        ).literal
      )
    }
  }

  it should "parse simple infix expression, which combines only numbers" in {
    val input      = """
      5 + 5
      5 - 5
      5*5
      5 >= 5
      5 != 5
      5 == 5
    """
    val statements = new Parser(input).parse.statements
    assert(statements.length === 6)
    val zipped = statements.zip(
      List[TokenType](
        new PLUS,
        new MINUS,
        new TIMES,
        new GEQ,
        new NEQ,
        new EQ
      )
    )
    for ((statementRaw: Statement, expected: TokenType) <- zipped) {
      assert(statementRaw.isInstanceOf[ExpressionStatement])
      val expressionRaw =
        statementRaw.asInstanceOf[ExpressionStatement].expression
      assert(expressionRaw.isInstanceOf[InfixExpression])
      val expression = expressionRaw.asInstanceOf[InfixExpression]
      val (left, op, right) =
        (expression.left, expression.operatorToken, expression.right)
      assert(left.isInstanceOf[NumberLiteral])
      assert(right.isInstanceOf[NumberLiteral])
      assert(left.asInstanceOf[NumberLiteral].value == 5)
      assert(op.getClass === expected.getClass)
    }
  }
  it should "parse simple infix expression, which contains only identifier" in {
    val input      = "a-b"
    val statements = new Parser(input).parse.statements
    assert(statements.length == 1)
    val statementRaw = statements(0)
    assert(statementRaw.isInstanceOf[ExpressionStatement])
    val expressionRaw =
      statementRaw.asInstanceOf[ExpressionStatement].expression
    assert(expressionRaw.isInstanceOf[InfixExpression])
    val expression = expressionRaw.asInstanceOf[InfixExpression]
    assert(expression.left.isInstanceOf[Identifier])
    assert(expression.left.asInstanceOf[Identifier].value === "a")
    assert(expression.operatorToken.isInstanceOf[MINUS])
    assert(expression.right.isInstanceOf[Identifier])
    assert(expression.right.asInstanceOf[Identifier].value === "b")
  }

  def testInfixExpressions(tests: List[Tuple2[String, String]]): Unit = {
    tests.foreach(tuple => {
      val input        = tuple._1
      val expected     = tuple._2
      val statementRaw = new Parser(input).parse.statements(0)
      assert(statementRaw.isInstanceOf[ExpressionStatement])
      val expressionRaw =
        statementRaw.asInstanceOf[ExpressionStatement].expression
      assert(expressionRaw.isInstanceOf[InfixExpression])
      assert(
        expressionRaw.asInstanceOf[InfixExpression].toString() === expected
      )
    })
  }

  it should "parse complex infix expression" in {
    val tests = List[Tuple2[String, String]](
      ("-a*b", "((-a) * b)"),
      ("!-a + b", "((!(-a)) + b)"),
      ("a+b + c", "((a + b) + c)"),
      ("!!a + !!1", "((!(!a)) + (!(!1.0)))"),
      ("a+b/c", "(a + (b / c))"),
      ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
      ("5 > 4 == 3 < 4", "((5.0 > 4.0) == (3.0 < 4.0))"),
      (
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "((3.0 + (4.0 * 5.0)) == ((3.0 * 1.0) + (4.0 * 5.0)))"
      ),
      ("-1+1", "((-1.0) + 1.0)")
    )
    testInfixExpressions(tests)
  }

  it should "parse multiple infix expressions" in {
    val input      = """
    1+2
    1+2
    1+2
    """
    val statements = new Parser(input).parse.statements
    statements.foreach(statement => {
      assert(statement.isInstanceOf[ExpressionStatement])
      val expression = statement.asInstanceOf[ExpressionStatement].expression
      assert(expression.toString === "(1.0 + 2.0)")
    })
  }

  it should "parse boolean lets, returns, and expressions" in {
    val input = """
    true
    let x = false
    return true
    """

    val statements = new Parser(input).parse.statements

    assert(statements(0).isInstanceOf[ExpressionStatement])
    val exp0 = statements(0).asInstanceOf[ExpressionStatement].expression
    assert(exp0.isInstanceOf[BooleanLiteral])
    assert(exp0.asInstanceOf[BooleanLiteral].value == true)

    assert(statements(1).isInstanceOf[LetStatement])
    val exp1 = statements(1).asInstanceOf[LetStatement].expression
    assert(exp1.isInstanceOf[BooleanLiteral])
    assert(exp1.asInstanceOf[BooleanLiteral].value == false)

    assert(statements(2).isInstanceOf[ReturnStatement])
    val exp2 = statements(2).asInstanceOf[ReturnStatement].expression
    assert(exp2.isInstanceOf[BooleanLiteral])
    assert(exp2.asInstanceOf[BooleanLiteral].value == true)
  }

  it should "parse grouped expressions" in {
    val tests: List[Tuple2[String, String]] = List(
      ("(1+2)", "(1.0 + 2.0)"),
      ("1 + (2 + 3)", "(1.0 + (2.0 + 3.0))"),
      ("1*(2+3)", "(1.0 * (2.0 + 3.0))"),
      ("!(1+2)+3", "((!(1.0 + 2.0)) + 3.0)")
    )
    testInfixExpressions(tests)
  }

  it should "parse if expression without else" in {
    val input        = "if(x>y){x}"
    val statementRaw = new Parser(input).parse.statements(0)
    assert(statementRaw.isInstanceOf[ExpressionStatement])
    val expressionRaw =
      statementRaw.asInstanceOf[ExpressionStatement].expression
    assert(expressionRaw.isInstanceOf[IfExpression])
    val exp = expressionRaw.asInstanceOf[IfExpression]
    assert(exp.condition.isInstanceOf[InfixExpression])
    assert(exp.condition.asInstanceOf[InfixExpression].toString === "(x > y)")
    val thenStmt = exp.thenBlock.statements(0)
    assert(
      thenStmt
        .isInstanceOf[ExpressionStatement]
    )
    assert(
      thenStmt
        .asInstanceOf[ExpressionStatement]
        .expression
        .isInstanceOf[Identifier]
    )
    assert(exp.elseBlock.isInstanceOf[EmptyBlockStatement])
    assert(exp.toString() === """if (x > y)
then {
x
}
else {
}
""")
  }

  it should "parse if statement with else branch" in {
    val input        = "if (x<y) {x} else {y}"
    val statementRaw = new Parser(input).parse.statements(0)
    assert(statementRaw.isInstanceOf[ExpressionStatement])
    val expressionRaw =
      statementRaw.asInstanceOf[ExpressionStatement].expression
    assert(expressionRaw.isInstanceOf[IfExpression])
    val elseBlock = expressionRaw.asInstanceOf[IfExpression].elseBlock
    assert(elseBlock.statements(0).isInstanceOf[ExpressionStatement])
    val exp =
      elseBlock.statements(0).asInstanceOf[ExpressionStatement].expression
    assert(exp.isInstanceOf[Identifier])
    assert(exp.asInstanceOf[Identifier].value === "y")
  }

  it should "parse function literal" in {
    val input        = """
    func(x, y){
      return x
    }
    """
    val statementRaw = new Parser(input).parse.statements(0)
    assert(statementRaw.isInstanceOf[ExpressionStatement])
    val expressionRaw =
      statementRaw.asInstanceOf[ExpressionStatement].expression
    assert(expressionRaw.isInstanceOf[FunctionLiteral])
    val expression = expressionRaw.asInstanceOf[FunctionLiteral]
    expression.parameters
      .zip(
        List(
          new Identifier(new IDENTIFIER("x")),
          new Identifier(new IDENTIFIER("y"))
        )
      )
      .foreach(x => {
        assert(x._1.value === x._2.value && x._1.getClass() === x._2.getClass())
      })
    assert(expression.block.statements(0).isInstanceOf[ReturnStatement])
    assert(expression.toString() === """func(x, y){
return x
}
""")
  }

  it should "parse different numbers of parameters" in {
    val input = """
    func(){}
    func(x){}
    func(x,y,z){}
    """

    val expectedNum = List(0, 1, 3)
    val statements  = new Parser(input).parse.statements
    assert(statements.length === expectedNum.length)
    statements
      .zip(expectedNum)
      .foreach(x => {
        assert(x._1.isInstanceOf[ExpressionStatement])
        assert(
          x._1
            .asInstanceOf[ExpressionStatement]
            .expression
            .asInstanceOf[FunctionLiteral]
            .parameters
            .length === x._2
        )
      })
  }
}
