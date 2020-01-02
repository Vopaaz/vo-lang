package volang.eval

import org.scalatest.FlatSpec
import volang.ast.Parser
import scala.reflect._

class EvalSpec extends FlatSpec {

  def checkInputExpected[T1 <: VoObject: ClassTag](
      inputExpected: List[Tuple2[String, Any]]
  ) = {
    inputExpected.foreach(x => {
      val root = new Parser(x._1).parse
      val obj  = Evaluator.evaluate(root)
      assert(classTag[T1].runtimeClass.isInstance(obj))
      assert(obj.asInstanceOf[T1].value == x._2)
    })
  }

  "Evaluator" should "evaluate number expression" in {
    val inputExpected = List(
      ("5", 5),
      ("10.5", 10.5)
    )
    checkInputExpected[VoNumber](inputExpected)
  }

  it should "evaluate boolean expression" in {
    val inputs   = List("true", "false")
    val expected = List(true, false)
    checkInputExpected[VoBoolean](inputs.zip(expected))
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

    checkInputExpected[VoBoolean](inputExpected)
  }

  it should "evaluate prefix expression for '-'" in {
    val inputExpected = List(
      ("-1", -1),
      ("--2", 2),
      ("-0", 0)
    )

    checkInputExpected[VoNumber](inputExpected)
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

    checkInputExpected[VoBoolean](inputExpected)
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

    checkInputExpected[VoNumber](inputExpected)
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
      ("if(0){0}else{1}", 1)
    )

    checkInputExpected[VoNumber](inputExpected)
    assert(
      Evaluator.evaluate(new Parser("if(false){1}").parse).isInstanceOf[VoNone]
    )
  }

  it should "return VoError object when encounter error" in {
    assert(Evaluator.evaluate(new Parser("1+true").parse).isInstanceOf[VoError])
  }

  it should "stop evaluating a set of statements when encounter error" in {
    List("""
      9
      5
      1+true
      10
      """, """
      if(1>0){
        3
        12
        1+true
        5
      }
      """).foreach(x => {
      assert(Evaluator.evaluate(new Parser(x).parse).isInstanceOf[VoError])
    })
  }

  it should "evaluate identifiers whose value is assigned" in {
    val inputExpected = List(
      ("""
    let a = 5
    a""", 5),
      ("""
    let a = 5+5
    a
    """, 10),
      ("""
    let a = 5
    let b = a
    b""", 5),
      ("""
    let a = 5
    let b = a+1
    let c = a+b
    """, 11)
    )

    checkInputExpected[VoNumber](inputExpected)
  }

  it should "return VoError if a value hasn't been bounded to an identifier" in {
    assert(Evaluator.evaluate(new Parser("foo").parse).isInstanceOf[VoError])
  }

  it should "evaluate function literal" in {
    val input  = """func(x,y){
      x
      y
    }"""
    val result = Evaluator.evaluate(new Parser(input).parse)
    assert(result.isInstanceOf[VoFunction])
    val f = result.asInstanceOf[VoFunction]
    assert(f.parameters.length == 2)
    assert(f.block.statements.length == 2)
  }

  it should "evaluate parameter binding and function call" in {
    val inputExpected = List(
      ("""
      let f = func(x){x}
      f(1)
      """, 1),
      ("""
      func(x){x}(1)
      """, 1)
    )

    checkInputExpected[VoNumber](inputExpected)
  }

  it should "evaluate complex function call" in {
    val inputExpected = List(
      ("""
      let f = func(x){
        x + 1
      }
      f(1)
      """, 2),
      ("""
      let f = func(x, y){
        if(x < y){
          1
        } else {
          2
        }
      }
      f(1,2)
      """, 1),
      ("""
      let f = func(x, y){
        let z = x
        if (z > y){
          1
        } else {
          2
        }
      }
      f(3,2)
      """, 1),
      ("""
      func(x){
        2*x
      }(2)
      """, 4),
      ("""
      let add = func(x, y){ x + y }
      add(1, add(1, 1))
      """, 3),
      ("""
      let add = func(x, y){ x + y }
      add(add(1*2, 1+2), add(1/2, 1-2))
      """, 4.5)
    )

    checkInputExpected[VoNumber](inputExpected)
  }
}
