package volang.eval
import volang.ast._

object Evaluator {
  def evaluate(node: Node): VoObject = {
    node match {
      case root: Root            => evalStatements(root.statements)
      case stmt: Statement       => evalStatement(stmt)
      case number: NumberLiteral => new VoNumber(number.value)
      case bool: BooleanLiteral  => new VoBoolean(bool.value)
    }
  }

  private def evalStatements(statements: List[Statement]): VoObject = {
    statements.map(x => evalStatement(x)).last
  }

  private def evalStatement(statement: Statement): VoObject = {
    statement match {
      case expStmt: ExpressionStatement => evaluate(expStmt.expression)
    }
  }
}
