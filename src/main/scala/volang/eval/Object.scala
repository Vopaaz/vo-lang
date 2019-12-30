package volang.eval

abstract class VoObject(val value: Any) {
  val typeName: String = "VoObject"
  def >=(other: VoObject): VoBoolean = {
    throw new UndefinedOperatorException(typeName, ">=")
  }

  def <=(other: VoObject): VoBoolean = {
    throw new UndefinedOperatorException(typeName, "<=")
  }

  def >(other: VoObject): VoBoolean = {
    throw new UndefinedOperatorException(typeName, ">")
  }

  def <(other: VoObject): VoBoolean = {
    throw new UndefinedOperatorException(typeName, "<")
  }

  def ==(other: VoObject): VoBoolean = {
    throw new UndefinedOperatorException(typeName, "==")
  }

  def !=(other: VoObject): VoBoolean = {
    !(==(other))
  }

  def +(other: VoObject): VoObject = {
    throw new UndefinedOperatorException(typeName, "+")
  }

  def -(other: VoObject): VoObject = {
    throw new UndefinedOperatorException(typeName, "-")
  }

  def *(other: VoObject): VoObject = {
    throw new UndefinedOperatorException(typeName, "*")
  }

  def /(other: VoObject): VoObject = {
    throw new UndefinedOperatorException(typeName, "/")
  }
}

class VoNumber(override val value: Double) extends VoObject {
  override val typeName = "Number"
  override def toString(): String = {
    value.toString()
  }

  override def >=(other: VoObject): VoBoolean = {
    assert(other.isInstanceOf[VoNumber])
    new VoBoolean(value >= other.asInstanceOf[VoNumber].value)
  }

  override def <=(other: VoObject): VoBoolean = {
    assert(other.isInstanceOf[VoNumber])
    new VoBoolean(value <= other.asInstanceOf[VoNumber].value)
  }

  override def <(other: VoObject): VoBoolean = {
    assert(other.isInstanceOf[VoNumber])
    new VoBoolean(value < other.asInstanceOf[VoNumber].value)
  }

  override def >(other: VoObject): VoBoolean = {
    assert(other.isInstanceOf[VoNumber])
    new VoBoolean(value > other.asInstanceOf[VoNumber].value)
  }

  override def ==(other: VoObject): VoBoolean = {
    new VoBoolean(if (other.isInstanceOf[VoNumber]) {
      value == other.asInstanceOf[VoNumber].value
    } else {
      false
    })
  }

  override def -(other: VoObject): VoNumber = {
    assert(other.isInstanceOf[VoNumber])
    new VoNumber(value - other.asInstanceOf[VoNumber].value)
  }
}

class VoBoolean(override val value: Boolean) extends VoObject {
  override val typeName = "Boolean"
  override def toString(): String = {
    value.toString()
  }
  override def ==(other: VoObject) = {
    assert(other.isInstanceOf[VoBoolean])
    new VoBoolean(value == other.asInstanceOf[VoBoolean].value)
  }

  def unary_! = {
    new VoBoolean(!value)
  }
}

class VoNone extends VoObject {
  override val value    = None
  override val typeName = "None"
  override def toString(): String = {
    "none"
  }

  override def ==(other: VoObject) = {
    new VoBoolean(other.isInstanceOf[VoNone])
  }
}
