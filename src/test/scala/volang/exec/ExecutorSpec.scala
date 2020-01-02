package volang.exec

import org.scalatest.FlatSpec
import java.io.ByteArrayOutputStream

class ExecutorSpec extends FlatSpec {
  "Executor" should "work" in {
    val input = """
      let adder = func(x) {
        func(y) { x + y }
      }

      let add2 = adder(2)
      add2(2)
      """
    val out   = new ByteArrayOutputStream
    Console.withOut(out) {
      Executor.exec(input)
      assert(out.toString().contains("4.0"))
    }
  }
}
