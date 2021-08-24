package io.github.fluent4s.rst

trait ResolvedPattern {

  type RPattern = List[RPatternElement]

  sealed class RPatternElement

  case class RTextElement(value: String) extends RPatternElement

  case class RBlockTextElement(value: Option[String]) extends RPatternElement

  case class RPlaceable(expression: RExpression) extends RPatternElement
}
