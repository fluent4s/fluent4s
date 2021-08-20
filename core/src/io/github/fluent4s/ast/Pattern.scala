package io.github.fluent4s.ast

trait Pattern {

  /**
   * Fluent pattern contains the value of a [[FMessage]], a [[FTerm]] or a [[FAttribute]].
   *
   * Each [[FPattern]] is a list of [[FPatternElement]] nodes representing either a textual
   * value or a combinaison of placeholder nodes and text literals hat make a translation.
   *
   * NOTE: A pattern with a [[BlockTextElement]] followed by a [[TextElement]] is invalid.
   * If a [[BlockTextElement]] is unable to parse the content of the following [[TextElement]],
   * it means it stops due to an indented char (those cannot be: `*`, `[`, `.`). Therefore,
   * it is an invalid parser behavior.
   *
   * @param elements List of [[FPatternElement]].
   */
  sealed class FPattern(val elements: List[FPatternElement])

  /**
   * [[FPatternElement]] is an element of [[FPattern]]
   *
   * It represents either a textual value or a combinaison of placeholder nodes and text
   * literals that make a part of a translation.
   *
   * See [[TextElement]] for inline text literals.
   * See [[BlockTextElement]] for block text literals.
   * See [[Placeable]] for Inline Expression and Select Expression.
   */
  sealed abstract class FPatternElement

  /**
   * [[TextElement]] represents an inline textual value.
   *
   * @param value The inline textual value.
   */
  case class TextElement(value: String) extends FPatternElement

  /**
   * [[BlockTextElement]] represents an inline textual value.
   *
   * @param value The textual value (or None if it is a blank line).
   * @param indent The indentation of the last line.
   */
  case class BlockTextElement(indent: Int, value: Option[String]) extends FPatternElement

  /**
   * [[Placeable]] represent a placeholder.
   *
   * There is two kinds of placeholder according to their inner expression.
   * See [[FExpression]].
   *
   * @param expression The inner expression ([[FExpression]])
   */
  case class Placeable(expression: FExpression) extends FPatternElement

}