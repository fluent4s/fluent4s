package fluent_syntax.ast

import cats.Show
import cats.implicits._
import scala.collection.mutable

/* TODO: Documentation needed!!!! */
sealed class FResource(val body: List[FEntry]);
implicit val showFResource: Show[FResource] = Show.show("FResource {\nbody: [\n"+_.body.map(_.show).mkString(",\n")+"\n]\n}")

sealed abstract class FEntry
implicit val showFEntry: Show[FEntry] = Show.show({
    case Junk(content) => s"Junk(content: $content)"
    case Message(body) => s"Message(body: ${body.show})"
    case Term(body) => s"Term(body: ${body.show})"
    case Comment(body) => s"Comment(body: ${body.show})"
    case GroupComment(body) => s"GroupComment(body: ${body.show})"
    case ResourceComment(body) => s"ResourceComment(body: ${body.show})"
})

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
implicit val showFMessage: Show[FMessage] = Show.show(mes => s"FMessage {\nidentifier: ${mes.id.show},\nvalue: ${mes.value.show},\nattributes: [\n"+mes.attributes.map(_.show).mkString(",\n")+s"\n],\ncomments: ${mes.comments.show}\n}")

sealed class FTerm(
    val id: FIdentifier,
    val value: FPattern,
    val attributes: List[FAttribute],
    val comments: Option[FComment]
)
implicit val showFTerm: Show[FTerm] = Show.show(term => s"FTerm {\nidentifier: ${term.id.show},\nvalue: ${term.value.show},\nattributes: [\n"+term.attributes.map(_.show).mkString(",\n")+s"\n],\ncomments: ${term.comments.show}\n}")

sealed class FComment(val content: String)
implicit val showFComment: Show[FComment] = Show.show(_.content)

sealed class FPattern(val elements: List[FPatternElement])
implicit val showFPattern: Show[FPattern] = Show.show("FPattern {\nelements: [\n"+_.elements.map(_.show).mkString(",\n")+"\n]\n}")

sealed abstract class FPatternElement
implicit val showFPatternElement: Show[FPatternElement] = Show.show({
    case TextElement(value) => s"TextElement(value: $value)"
    case BlockTextElement(indent, value) => s"BlockTextElement(indent: ${indent.show}, value: ${value.show})"
    case Placeable(expression) => s"Placeable(expression: ${expression.show})"
})
case class TextElement(value: String) extends FPatternElement
case class BlockTextElement(indent: Int, value: Option[String])
    extends FPatternElement
case class Placeable(expression: FExpression) extends FPatternElement

sealed class FAttribute(val id: FIdentifier, val value: FPattern)
implicit val showFAttribute: Show[FAttribute] = Show.show(attr => s"FAttribute {\nidentifier: ${attr.id.show},\nvalue: ${attr.value.show}\n}")

sealed class FIdentifier(val name: String)
implicit val showFIdentifier: Show[FIdentifier] = Show.show(_.name)

sealed class FVariant(
    val key: FVariantKey,
    val value: FPattern,
    val default: Boolean
)
implicit val showFVariant: Show[FVariant] = Show.show(var_ => s"FVariant {\nkey: ${var_.key.show},\nvalue: ${var_.value.show},\ndefault: ${var_.default.show}\n}")

sealed abstract class FVariantKey
case class IdentifierKey(val value: FIdentifier) extends FVariantKey
case class NumberLiteralKey(val value: String) extends FVariantKey
implicit val showFVariantKey: Show[FVariantKey] = Show.show({
    case IdentifierKey(value) => s"IdentifierKey(value: ${value.show})"
    case NumberLiteralKey(value) => s"NumberLiteralKey(value: $value)"
})

sealed class FCallArguments(
    val positional: List[FInlineExpression],
    val named: List[NamedArgument]
)
implicit val showFCallArguments: Show[FCallArguments] = Show.show(args => s"FCallArguments {\npositional: [\n"+args.positional.map(_.show).mkString(",\n")+"\n],\nnamed: [\n"+args.named.map(_.show).mkString(",\n")+"\n],\n}")

sealed abstract class FArgument
implicit val showFArgument: Show[FArgument] = Show.show({
    case PositionalArgument(value) => s"PositionalArgument{\nvalue: ${value.show}\n}"
    case NamedArgument(name, value) => s"NamedArgument{\nname: ${name.show},\nvalue: ${value.show}\n)"
})
case class PositionalArgument(
    val value: FInlineExpression
) extends FArgument
case class NamedArgument(
    val name: FIdentifier,
    val value: FInlineExpression
) extends FArgument
implicit val showNamedArgument: Show[NamedArgument] = Show.show(arg => s"NamedArgument{\nname: ${arg.name.show},\nvalue: ${arg.value.show}\n}"
)

sealed abstract class FInlineExpression
implicit val showFInlineExpression: Show[FInlineExpression] = Show.show({
    case StringLiteral(value) => s"StringLiteral(value: $value)"
    case NumberLiteral(value) => s"NumberLiteral(value: $value)"
    case FunctionReference(id, args) => s"FunctionReference(identifier: ${id.show}, arguments: ${args.show})"
    case MessageReference(id, attribute) => s"MessageReference(identifier: ${id.show}, attribute: ${attribute.show})"
    case TermReference(id, attribute, args) => s"TermReference(identifier: ${id.show}, attribute: ${attribute.show}, arguments: ${args.show})"
    case VariableReference(id) => s"VariableReference(identifier: ${id.show})"
    case PlaceableExpr(expression) => s"PlaceableExpr(expression: ${expression.show})"
})
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
implicit val showFExpression: Show[FExpression] = Show.show({
    case Select(selector, variants) => s"Select{\nselector: ${selector.show},\nvariants: ${variants.show}\n}"
    case Inline(body) => s"Inline{\nbody: ${body.show}\n}"
})
case class Select(val selector: FInlineExpression, val variants: List[FVariant])
    extends FExpression
case class Inline(val body: FInlineExpression) extends FExpression
