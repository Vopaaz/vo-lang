package volang.eval

abstract class VoObject(val value: Any)

class VoNumber(override val value: Double) extends VoObject {
  val typeName = "Number"
  override def toString(): String = {
    value.toString()
  }
}

class VoBoolean(override val value: Boolean) extends VoObject {
  val typeName = "Boolean"
  override def toString(): String = {
    value.toString()
  }
}

class VoNone extends VoObject {
  override val value = None
  override def toString(): String = {
    "none"
  }
}
