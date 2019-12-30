package volang.eval

import org.scalatest.FlatSpec
import volang.ast.Parser

class EvalSpec extends FlatSpec {
  "Evaluator" should "evaluate number expression" in {
    val inputs = List(
      "5",
      "10.5"
    )
    val expected = List(5, 10.5)
    inputs
      .zip(expected)
      .foreach(x => {
        val root = new Parser(x._1).parse
        val obj  = Evaluator.evaluate(root)
        assert(obj.isInstanceOf[VoNumber])
        assert(obj.asInstanceOf[VoNumber].value == x._2)
      })
  }

  it should "evaluate boolean expression" in {
    val inputs   = List("true", "false")
    val expected = List(true, false)
    inputs
      .zip(expected)
      .foreach(x => {
        val root = new Parser(x._1).parse
        val obj  = Evaluator.evaluate(root)
        assert(obj.isInstanceOf[VoBoolean])
        assert(obj.asInstanceOf[VoBoolean].value == x._2)
      })
  }

  it should "evaluate none expression" in {
    val input = "none"
    val root  = new Parser(input).parse
    val obj   = Evaluator.evaluate(root)
    assert(obj.isInstanceOf[VoNone])
  }

  it should "evaluate prefix expression for '!'" in {
    val inputExpected = List(
      ("!true", false),
      ("!!true", true),
      ("!!false", false),
      ("!1", false),
      ("!0", true),
      ("!!0", false)
    )

    inputExpected.foreach(x => {
      val root = new Parser(x._1).parse
      val obj  = Evaluator.evaluate(root)
      assert(obj.isInstanceOf[VoBoolean])
      assert(obj.asInstanceOf[VoBoolean].value == x._2)
    })
  }

  it should "evaluate prefix expression for '-'" in {
    val inputExpected = List(
      ("-1", -1),
      ("--2", 2),
      ("-0", 0)
    )

    inputExpected.foreach(x => {
      val root = new Parser(x._1).parse
      val obj  = Evaluator.evaluate(root)
      assert(obj.isInstanceOf[VoNumber])
      assert(obj.asInstanceOf[VoNumber].value == x._2)
    })
  }

  it should "evaluate comparison expression" in {
    val inputExpected = List(
      ("1<=1", true),
      ("1>2", false),
      ("true == true", true),
      ("3 != false", true),
      ("1>2 == false", true),
      ("1>2 != 2>3", false)
    )

    inputExpected.foreach(x => {
      val root = new Parser(x._1).parse
      val obj  = Evaluator.evaluate(root)
      assert(obj.isInstanceOf[VoBoolean])
      assert(obj.asInstanceOf[VoBoolean].value == x._2)
    })
  }

  it should "evaluate number arithmetic expression" in {
    val inputExpected = List(
      ("1+1", 2),
      ("2*5 -1", 9),
      ("10 - 2/2", 9),
      ("(1+1) * 10", 20),
      ("1/10", 0.1),
      ("(1+(-2))/10", -0.1),
      ("(1+-1)/10", 0.0)
    )

    inputExpected.foreach(x => {
      val root = new Parser(x._1).parse
      val obj  = Evaluator.evaluate(root)
      assert(obj.isInstanceOf[VoNumber])
      assert(obj.asInstanceOf[VoNumber].value == x._2)
    })
  }

  it should "evaluate if expressions" in {
    val inputExpected = List(
      ("if(true){1}", 1),
      ("if(1>0){1}", 1),
      ("if(1>10) {0} else {1}", 1),
      ("""
      if(1 < 2){
        1
      } else{
        10
      }
      """, 1),
      ("if(1){1}", 1),
      ("if(0){0}else{1}",1)
    )

    inputExpected.foreach(x => {
      val root = new Parser(x._1).parse
      val obj  = Evaluator.evaluate(root)
      assert(obj.isInstanceOf[VoNumber])
      assert(obj.asInstanceOf[VoNumber].value == x._2)
    })
    assert(
      Evaluator.evaluate(new Parser("if(false){1}").parse).typeName === "None"
    )
  }
}
