package io.github.fluent4s.rst

trait ResolvedBase {

  case class RResource(body: Map[String, REntry])

  sealed class RExpression

  case class RSelect(selector: RInlineExpression, variants: List[RVariant])

  case class RInline(body: RInlineExpression)
}