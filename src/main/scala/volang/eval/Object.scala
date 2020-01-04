package volang.eval
import volang.ast._

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

  def asBoolean: VoBoolean = {
    throw new UndefinedOperatorException(typeName, "asBoolean")
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

  override def +(other: VoObject): VoNumber = {
    assert(other.isInstanceOf[VoNumber])
    new VoNumber(value + other.asInstanceOf[VoNumber].value)
  }

  override def -(other: VoObject): VoNumber = {
    assert(other.isInstanceOf[VoNumber])
    new VoNumber(value - other.asInstanceOf[VoNumber].value)
  }

  override def *(other: VoObject): VoNumber = {
    assert(other.isInstanceOf[VoNumber])
    new VoNumber(value * other.asInstanceOf[VoNumber].value)
  }

  override def /(other: VoObject): VoNumber = {
    assert(other.isInstanceOf[VoNumber])
    new VoNumber(value / other.asInstanceOf[VoNumber].value)
  }

  override def asBoolean: VoBoolean = {
    if (value == 0) {
      new VoBoolean(false)
    } else {
      new VoBoolean(true)
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
    new VoBoolean(value == other.asInstanceOf[VoBoolean].value)
  }

  def unary_! = {
    new VoBoolean(!value)
  }

  override def asBoolean: VoBoolean = {
    this
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

  override def asBoolean: VoBoolean = {
    new VoBoolean(false)
  }
}

class VoFunction(
    val parameters: List[Identifier],
    val block: BlockStatement,
    val environment: Environment
) extends VoObject {

  override val typeName: String = "Function"

  override def toString(): String = {
    "func(" +
      parameters
        .map(x => x.value)
        .mkString(", ") +
      ")" + block.toString()
  }
}

class VoError(val message: String) extends VoObject {
  override val value: Any       = None
  override val typeName: String = "Error"
  override def toString(): String = {
    message
  }
}

class VoString(override val value: String) extends VoObject {
  override val typeName: String = "String"
  override def toString(): String = {
    value
  }

  override def ==(other: VoObject): VoBoolean = {
    assert(other.isInstanceOf[VoString])
    new VoBoolean(value == other.asInstanceOf[VoString].value)
  }

  override def asBoolean: VoBoolean = {
    new VoBoolean(value.nonEmpty)
  }

  override def +(other: VoObject): VoString = {
    assert(other.isInstanceOf[VoString])
    new VoString(value + other.asInstanceOf[VoString].value)
  }
}
