package volang.exec

import org.scalatest.FlatSpec
import java.io.ByteArrayOutputStream
import scala.io.Source

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

  it should "run every demo" in {
    val csv =
      Source.fromResource("demo/index.csv").getLines().filter(_.nonEmpty)
    csv.next() // Skip the header

    csv.foreach(line => {
      val split        = line.split(",")
      val filename     = split(0).trim()
      val result       = split(1).trim()
      val fullFilename = s"demo/$filename.vo"
      val content      = Source.fromResource(fullFilename).mkString
      val out          = new ByteArrayOutputStream
      Console.withOut(out) {
        Executor.exec(content)
        assert(out.toString().trim() == result)
      }
    })
  }
}
