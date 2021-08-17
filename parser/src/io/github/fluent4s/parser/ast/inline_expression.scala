package io.github.fluent4s.parser.ast

sealed abstract class FInlineExpression

case class StringLiteral(val value: String) extends FInlineExpression

case class NumberLiteral(val value: String) extends FInlineExpression

case class FunctionReference(val id: FIdentifier, val arguments: FCallArguments)
    extends FInlineExpression

case class MessageReference(
    val id: FIdentifier,
    val attribute: Option[FIdentifier]
) extends FInlineExpression

case class TermReference(
    val id: FIdentifier,
    val attribute: Option[FIdentifier],
    val arguments: Option[FCallArguments]
) extends FInlineExpression

case class VariableReference(val id: FIdentifier) extends FInlineExpression

case class PlaceableExpr(expression: FExpression) extends FInlineExpression

sealed abstract class FExpression

case class Select(val selector: FInlineExpression, val variants: List[FVariant]) extends FExpression

case class Inline(val body: FInlineExpression) extends FExpression

sealed class FCallArguments(
    val positional: List[FInlineExpression],
    val named: List[NamedArgument]
)

sealed abstract class FArgument

case class PositionalArgument(
    val value: FInlineExpression
) extends FArgument

case class NamedArgument(
    val name: FIdentifier,
    val value: FInlineExpression
) extends FArgument
