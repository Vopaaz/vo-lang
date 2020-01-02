package volang.repl

import org.scalatest.FlatSpec
import scala.io._
import java.io.StringBufferInputStream
import java.io.ByteArrayOutputStream

class ReplSpec extends FlatSpec {
  "Testing Suite" should "work" in {
    val s  = "in"
    val in = new StringBufferInputStream(s)

    Console.withIn(in) {
      assert(StdIn.readLine() === s)
    }

    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      print(s)
      assert(out.toString() === s)
    }
  }

  "ReadLoop" should "exit when user input quit" in {
    var in  = new StringBufferInputStream(":q")
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        ReadLoop.startREPL
      }

      in = new StringBufferInputStream(":exit")
      Console.withIn(in) {
        ReadLoop.startRLPL
      }

      in = new StringBufferInputStream(":quit")
      Console.withIn(in) {
        ReadLoop.startRPPL
      }
    }
  }

  it should "run smoothly in basic inputs" in {
    val input = """
    1
    !true
    -2
    :q
    """

    var in  = new StringBufferInputStream(input)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        ReadLoop.startREPL
      }

      in = new StringBufferInputStream(input)
      Console.withIn(in) {
        ReadLoop.startRLPL
      }

      in = new StringBufferInputStream(input)
      Console.withIn(in) {
        ReadLoop.startRPPL
      }
    }
  }

  it should "evaluate continuously (without clearing the environment)" in {
    val input = """
    let x = 5

    x
    
    :q
    """

    val in  = new StringBufferInputStream(input)
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withIn(in) {
        ReadLoop.startREPL
      }
      val outStr = out.toString()
      assert(outStr.contains("5.0"))
      assert(
        outStr.slice(outStr.indexOf("5.0") + 2, outStr.length()).contains("5.0")
      )
    }
  }
}
