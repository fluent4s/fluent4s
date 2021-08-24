package io.github.fluent4s.rst

trait ResolvedInlineExpression {

  sealed class RInlineExpression

  case class RStringLiteral(value: String) extends RInlineExpression

  case class RNumberLiteral(value: Double) extends RInlineExpression

  case class RMessageReference(resolved: RMessage) extends RInlineExpression

  case class RAttributeReference(resolved: RPattern) extends RInlineExpression

  case class RTermReference(resolved: RTerm, arguments: Option[RCallArguments]) extends RInlineExpression

  case class RVariableReference(id: String) extends RInlineExpression

  case class RCallArguments(positional: List[RInlineExpression], named: Map[String, RInlineExpression])

}
