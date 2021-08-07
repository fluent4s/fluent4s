package fluent_syntax.ast

import scala.collection.mutable

/* TODO: Documentation needed!!!! */
sealed class FResource(body: List[FEntry]);

sealed abstract class FEntry
case class Junk(content: String) extends FEntry
case class Message(body: FMessage) extends FEntry
case class Term(body: FTerm) extends FEntry
case class Comment(body: FComment) extends FEntry
case class GroupComment(body: FComment) extends FEntry
case class ResourceComment(body: FComment) extends FEntry

sealed class FMessage(
    val id: FIdentifier,
    val value: Option[FPattern],
    val attributes: List[FAttribute],
    val comments: Option[FComment]
)

sealed class FTerm(
    val id: FIdentifier,
    val value: FPattern,
    val attributes: List[FAttribute],
    val comments: Option[FComment]
)

sealed class FComment(val content: String)

sealed class FPattern(elements: List[FPatternElement])

sealed abstract class FPatternElement
case class TextElement(value: String) extends FPatternElement
case class BlockTextElement(ident: Int, value: Option[String]) extends FPatternElement
case class Placeable(expression: FExpression) extends FPatternElement

sealed class FAttribute(val id: FIdentifier, val value: FPattern)

sealed class FIdentifier(val name: String)

sealed class FVariant(
    val key: FVariantKey,
    val value: FPattern,
    val default: Boolean
)

sealed abstract class FVariantKey
case class IdentifierKey(val value: FIdentifier) extends FVariantKey
case class NumberLiteralKey(val value: String) extends FVariantKey

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
case class Select(val selector: FInlineExpression, val variants: List[FVariant])
    extends FExpression
case class Inline(val body: FInlineExpression) extends FExpression
