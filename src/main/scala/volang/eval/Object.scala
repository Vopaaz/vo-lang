package volang.eval

abstract class VoObject

class VoNumber(val value: Double) extends VoObject {
  val typeName = "Number"
  override def toString(): String = {
    value.toString()
  }
}

class VoBoolean(val value: Boolean) extends VoObject {
  val typeName = "Boolean"
  override def toString(): String = {
    value.toString()
  }
}

class VoNone extends VoObject {
  val value = None
  override def toString(): String = {
    "none"
  }
}
