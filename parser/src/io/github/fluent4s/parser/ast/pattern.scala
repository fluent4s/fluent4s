package io.github.fluent4s.parser.ast

sealed class FPattern(val elements: List[FPatternElement])

sealed abstract class FPatternElement

case class TextElement(value: String) extends FPatternElement

case class BlockTextElement(indent: Int, value: Option[String]) extends FPatternElement

case class Placeable(expression: FExpression) extends FPatternElement
