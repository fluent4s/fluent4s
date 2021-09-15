package io.github.fluent4s.ast

import cats.Show
import cats.implicits._

trait ASTBase {


  /**
   * Root node of a Fluent Translation list .
   *
   * A [[FResource]] contain a body with a list of [[FEntry]] nodes.
   *
   * @param body : List of [[FEntry]].
   */
  case class FResource(body: List[FEntry])

  /**
   * Attributes are part of a [[FMessage]] or a [[FTerm]].
   *
   * Attributes expressed list of keyedd [[FPattern]] on a [[FEntry]].
   *
   * @param id    Unique [[FIdentifier]] within a [[FResource]].
   * @param value [[Pattern]] as a translation with this attribute.
   */
  case class FAttribute(id: FIdentifier, value: FPattern)

  /**
   * Identifier is part of nodes such as [[FMessage]], [[FTerm]] and [[FAttribute]].
   *
   * It associate a unique key with a [[FTerm]], a [[FMessage]] or a [[FAttribute]]
   * to be used in [[FExpression]] as a way to refer to another [[FEntry]].
   *
   * @param name A unique identifier (it should respect the form [A-Za-z][A-Za-z0-9-_]*)
   */
  case class FIdentifier(name: String)

  /**
   * An expression that is either a select expression or an inline expression.
   *
   * See [[Select]] and [[Inline]].
   */
  sealed abstract class FExpression

  /**
   * A select expression based upon the evaluation of a [[FInlineExpression]].
   *
   * Upon the evaluation of a selector (a [[FInlineExpression]]), it selects a
   * variant (a [[FVariant]]) to express.
   *
   * @param selector An [[FInlineExpression]], usually a [[VariableReference]] to
   *                 be used as a selector.
   * @param variants List of possible [[FVariant]] to express
   */
  case class Select(selector: FInlineExpression, variants: List[FVariant])
    extends FExpression

  /**
   * A [[FInlineExpression]] part of a [[FExpression]].
   *
   * @param body The corresponding [[FInlineExpression]].
   */
  case class Inline(val body: FInlineExpression) extends FExpression

  /* Show implicits */
  implicit val showFResource: Show[FResource] =
    Show.show("FResource {\nbody: [\n" + _.body.map(_.show).mkString(",\n") + "\n]\n}")
  implicit val showFAttribute: Show[FAttribute] =
    Show.show(attr => s"FAttribute {\nidentifier: ${attr.id.show},\nvalue: ${attr.value.show}\n}")

  implicit val showFIdentifier: Show[FIdentifier] = Show.show(_.name)
  implicit val showFEntry: Show[FEntry] = Show.show({
    case Junk(content) => s"Junk(content: $content)"
    case Message(body) => s"Message(body: ${body.show})"
    case Term(body) => s"Term(body: ${body.show})"
    case Comment(body) => s"Comment(body: ${body.show})"
    case GroupComment(body) => s"GroupComment(body: ${body.show})"
    case ResourceComment(body) => s"ResourceComment(body: ${body.show})"
  })
  implicit val showFMessage: Show[FMessage] = Show.show(mes =>
    s"FMessage {\nidentifier: ${mes.id.show},\nvalue: ${mes.value.show},\nattributes: [\n" + mes.attributes.map(
      _.show
    ).mkString(",\n") + s"\n],\ncomments: ${mes.comments.show}\n}"
  )
  implicit val showFTerm: Show[FTerm] = Show.show(term =>
    s"FTerm {\nidentifier: ${term.id.show},\nvalue: ${term.value.show},\nattributes: [\n" + term.attributes.map(
      _.show
    ).mkString(",\n") + s"\n],\ncomments: ${term.comments.show}\n}"
  )
  implicit val showFComment: Show[FComment] = Show.show(_.content)
  implicit val showFPattern: Show[FPattern] =
    Show.show("FPattern {\nelements: [\n" + _.elements.map(_.show).mkString(",\n") + "\n]\n}")
  implicit val showFPatternElement: Show[FPatternElement] = Show.show({
    case TextElement(value) => s"TextElement(value: $value)"
    case BlockTextElement(indent, value) =>
      s"BlockTextElement(indent: ${indent.show}, value: ${value.show})"
    case Placeable(expression) => s"Placeable(expression: ${expression.show})"
  })
  implicit val showFInlineExpression: Show[FInlineExpression] = Show.show({
    case StringLiteral(value) => s"StringLiteral(value: $value)"
    case IntegerLiteral(value) => s"IntegerLiteral(value: $value)"
    case DecimalLiteral(value) => s"DecimalLiteral(value: $value)"
    case FunctionReference(id, args) =>
      s"FunctionReference(identifier: ${id.show}, arguments: ${args.show})"
    case MessageReference(id, attribute) =>
      s"MessageReference(identifier: ${id.show}, attribute: ${attribute.show})"
    case TermReference(id, attribute, args) =>
      s"TermReference(identifier: ${id.show}, attribute: ${attribute.show}, arguments: ${args.show})"
    case VariableReference(id) => s"VariableReference(identifier: ${id.show})"
    case PlaceableExpr(expression) => s"PlaceableExpr(expression: ${expression.show})"
  })
  implicit val showFExpression: Show[FExpression] = Show.show({
    case Select(selector, variants) =>
      s"Select{\nselector: ${selector.show},\nvariants: ${variants.show}\n}"
    case Inline(body) => s"Inline{\nbody: ${body.show}\n}"
  })
  implicit val showFVariant: Show[FVariant] = Show.show(var_ =>
    s"FVariant {\nkey: ${var_.key.show},\nvalue: ${var_.value.show},\ndefault: ${var_.default.show}\n}"
  )
  implicit val showFVariantKey: Show[FVariantKey] = Show.show({
    case IdentifierKey(value) => s"IdentifierKey(value: ${value.show})"
    case IntegerLiteralKey(value) => s"IntegerLiteralKey(value: $value)"
    case DecimalLiteralKey(value) => s"DecimalLiteralKey(value: $value)"
  })
  implicit val showFCallArguments: Show[FCallArguments] = Show.show(args =>
    s"FCallArguments {\npositional: [\n" + args.positional.map(_.show).mkString(
      ",\n"
    ) + "\n],\nnamed: [\n" + args.named.map(_.show).mkString(",\n") + "\n],\n}"
  )
  implicit val showFArgument: Show[FArgument] = Show.show({
    case PositionalArgument(value) => s"PositionalArgument{\nvalue: ${value.show}\n}"
    case NamedArgument(name, value) =>
      s"NamedArgument{\nname: ${name.show},\nvalue: ${value.show}\n)"
  })
  implicit val showNamedArgument: Show[NamedArgument] =
    Show.show(arg => s"NamedArgument{\nname: ${arg.name.show},\nvalue: ${arg.value.show}\n}")
}
