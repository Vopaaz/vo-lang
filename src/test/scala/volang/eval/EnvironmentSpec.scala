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

  it should "handle clone correctly" in {
    val env = new Environment
    env.set("n", new VoNumber(1))

    val newEnv = env.clone()
    assert(newEnv.get("n").isInstanceOf[VoNumber])

    env.set("n", new VoBoolean(true))
    assert(newEnv.get("n").isInstanceOf[VoNumber])

    newEnv.set("n", new VoNone)
    assert(newEnv.get("n").isInstanceOf[VoNone])
    assert(env.get("n").isInstanceOf[VoBoolean])
  }
}
