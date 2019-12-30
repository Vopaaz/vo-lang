package volang.eval

abstract class VoObject(val value: Any) {
  val typeName: String = "VoObject"
  def >=(other: VoObject): Boolean = {
    throw new UndefinedOperatorException(typeName, ">=")
  }

  def <=(other: VoObject): Boolean = {
    throw new UndefinedOperatorException(typeName, "<=")
  }

  def >(other: VoObject): Boolean = {
    throw new UndefinedOperatorException(typeName, ">")
  }

  def <(other: VoObject): Boolean = {
    throw new UndefinedOperatorException(typeName, "<")
  }

  def ==(other: VoObject): Boolean = {
    throw new UndefinedOperatorException(typeName, "==")
  }

  def !=(other: VoObject): Boolean = {
    !(==(other))
  }
}

class VoNumber(override val value: Double) extends VoObject {
  override val typeName = "Number"
  override def toString(): String = {
    value.toString()
  }

  override def >=(other: VoObject): Boolean = {
    assert(other.isInstanceOf[VoNumber])
    value >= other.asInstanceOf[VoNumber].value
  }

  override def <=(other: VoObject): Boolean = {
    assert(other.isInstanceOf[VoNumber])
    value <= other.asInstanceOf[VoNumber].value
  }

  override def <(other: VoObject): Boolean = {
    assert(other.isInstanceOf[VoNumber])
    value < other.asInstanceOf[VoNumber].value
  }

  override def >(other: VoObject): Boolean = {
    assert(other.isInstanceOf[VoNumber])
    value > other.asInstanceOf[VoNumber].value
  }

  override def ==(other: VoObject): Boolean = {
    if (other.isInstanceOf[VoNumber]) {
      value == other.asInstanceOf[VoNumber].value
    } else {
      false
    }
  }
}

class VoBoolean(override val value: Boolean) extends VoObject {
  override val typeName = "Boolean"
  override def toString(): String = {
    value.toString()
  }
  override def ==(other: VoObject) = {
    assert(other.isInstanceOf[VoBoolean])
    value == other.asInstanceOf[VoBoolean].value
  }
}

class VoNone extends VoObject {
  override val value    = None
  override val typeName = "None"
  override def toString(): String = {
    "none"
  }

  override def ==(other: VoObject) = {
    other.isInstanceOf[VoNone]
  }
}
