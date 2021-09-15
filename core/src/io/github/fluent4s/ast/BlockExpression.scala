package io.github.fluent4s.ast

trait BlockExpression {

  /**
   * A single branch of a [[Select]] expression.
   *
   * It's a pair of a [[FVariantKey]] and a [[FPattern]]. If the selector
   * match, the given pattern is returned as the value of the [[Select]]
   * expression.
   *
   * @param key the associated [[FVariantKey]].
   * @param value the associated [[FPattern]].
   * @param default If true, define itself as the default variant.
   */
  case class FVariant(key: FVariantKey, value: FPattern, default: Boolean)

  /**
   * A key of a [[FVariant]].
   *
   * Can either be an identifier (not a unique one) or a number.
   * See [[IdentifierKey]] and [[NumberLiteralKey]].
   */
  sealed trait FVariantKey

  /**
   * A identifier key of a [[FVariant]].
   *
   * Might not be unique.
   *
   * See [[FVariantKey]].
   *
   * @param value the inner [[FIdentifier]] to use as a key.
   */
  case class IdentifierKey(value: FIdentifier) extends FVariantKey

  /**
   * A number key of a [[FVariant]].
   *
   * See [[FVariantKey]].
   *
   * @param value the inner integer to use as a key.
   */
  case class IntegerLiteralKey(value: Long) extends FVariantKey

  /**
   * A number key of a [[FVariant]].
   *
   * See [[FVariantKey]].
   *
   * @param value the inner decimal number to use as a key.
   */
  case class DecimalLiteralKey(value: Double) extends FVariantKey

}