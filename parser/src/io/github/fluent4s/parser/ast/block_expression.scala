package io.github.fluent4s.parser.ast

sealed class FVariant(
    val key: FVariantKey,
    val value: FPattern,
    val default: Boolean
)

sealed abstract class FVariantKey

case class IdentifierKey(val value: FIdentifier) extends FVariantKey

case class NumberLiteralKey(val value: String) extends FVariantKey
