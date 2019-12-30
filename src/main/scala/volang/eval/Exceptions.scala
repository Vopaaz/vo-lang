package volang.eval

class UndefinedOperatorException(cls: String, operator: String)
    extends Exception {
  val message = s"Applying $operator on $cls is undefined."
}
