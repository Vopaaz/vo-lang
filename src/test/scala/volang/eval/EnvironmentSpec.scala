package volang.eval

import org.scalatest.FlatSpec

class EnvironmentSpec extends FlatSpec {
  "Environment" should "work" in {
    val env = new Environment
    assert(env.get("foo").isInstanceOf[VoError])
    env.set("foo", new VoNone)
    assert(env.get("foo").isInstanceOf[VoNone])
    env.set("foo", new VoNumber(1))
    assert(env.get("foo").isInstanceOf[VoNumber])

    env.clear
    assert(env.get("foo").isInstanceOf[VoError])
  }
}
