package volang.eval
import scala.collection.mutable._

class Environment {
  private val store = new HashMap[String, VoObject]
  def get(identifier: String): VoObject = {
    store.getOrElse(
      identifier,
      new VoError(s"'$identifier' has not been initialized")
    )
  }

  def set(identifier: String, value: VoObject) = {
    store(identifier) = value
  }

  def clear = {
    store.clear()
  }

  override def toString(): String = {
    store.mkString("\n")
  }

  override def clone(): Environment = {
    val newEnv = new Environment
    store.foreach(x => {
      newEnv.set(x._1, x._2)
    })
    newEnv
  }
}
