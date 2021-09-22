package io.github.fluent4s.ast

import cats.Eq
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
  sealed class FResource(val body: List[FEntry]);

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
  case class Select(val selector: FInlineExpression, val variants: List[FVariant])
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
    case NumberLiteral(value) => s"NumberLiteral(value: $value)"
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
    case NumberLiteralKey(value) => s"NumberLiteralKey(value: $value)"
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

  // Eq implicits
  implicit val eqFResource: Eq[FResource] =
    Eq.instance(_.body === _.body)
  implicit val eqFAttribute: Eq[FAttribute] =
    Eq.instance((x1, x2) => Eq.eqv(x1.id, x2.id) && (Eq.eqv(x1.value, x2.value)))

  implicit val eqFIdentifier: Eq[FIdentifier] = Eq.instance(_.name === _.name)
  implicit val eqFEntry: Eq[FEntry] = Eq.instance({
    case (Junk(c1), Junk(c2)) => c1 === c2
    case (Message(b1), Message(b2)) => b1 === b2
    case (Term(b1), Term(b2)) => b1 === b2
    case (Comment(b1), Comment(b2)) => b1 === b2
    case (GroupComment(b1), GroupComment(b2)) => b1 === b2
    case (ResourceComment(b1), ResourceComment(b2)) => b1 === b2
    case _ => false
  })
  implicit val eqFMessage: Eq[FMessage] = Eq.instance((m1, m2) =>
    m1.id === m2.id && m1.attributes == m2.attributes && m1.value == m2.value
  )
  implicit val eqFTerm: Eq[FTerm] =
    Eq.instance((m1, m2) =>
      m1.id === m2.id && m1.attributes == m2.attributes && m1.value == m2.value
    )
  implicit val eqFComment: Eq[FComment] = Eq.instance((c1, c2) => c1.content === c2.content)
  implicit val eqFPattern: Eq[FPattern] =
    Eq.instance((p1, p2) => p1.elements === p2.elements)
  implicit val eqFPatternElement: Eq[FPatternElement] = Eq.instance({
    case (TextElement(v1), TextElement(v2)) => v1 === v2
    case (BlockTextElement(id1, v1), BlockTextElement(id2, v2)) => id1 === id2 && v1 === v2
    case (Placeable(exp1), Placeable(exp2)) => exp1 === exp2
    case _ => false
  })
  implicit val eqFInlineExpression: Eq[FInlineExpression] = Eq.instance({
    case (StringLiteral(v1), StringLiteral(v2)) => v1 === v2
    case (NumberLiteral(v1), NumberLiteral(v2)) => v1 === v2
    case (FunctionReference(id1, args1), FunctionReference(id2, args2)) =>
      id1 === id2 && args1 === args2
    case (MessageReference(id1, attribute1), MessageReference(id2, attribute2)) =>
      id1 === id2 && attribute1 === attribute2
    case (TermReference(id1, attribute1, args1), TermReference(id2, attribute2, args2)) =>
      id1 === id2 && args1 === args2 && attribute1 === attribute2
    case (VariableReference(id1), VariableReference(id2)) => id1 === id2
    case (PlaceableExpr(exp1), PlaceableExpr(exp2)) => exp1 === exp2
    case _ => false
  })
  implicit val eqFExpression: Eq[FExpression] = Eq.instance({
    case (Select(selector1, variants1), Select(selector2, variants2)) =>
      selector1 === selector2 && variants1 === variants2
    case (Inline(b1), Inline(b2)) => b1 === b2
    case _ => false
  })
  implicit val eqFVariant: Eq[FVariant] =
    Eq.instance((var1, var2) => var1.key === var2.key && var1.value === var2.value)
  implicit val eqFVariantKey: Eq[FVariantKey] = Eq.instance({
    case (IdentifierKey(v1), IdentifierKey(v2)) => v1 === v2
    case (NumberLiteralKey(v1), NumberLiteralKey(v2)) => v1 === v2
    case _ => false
  })
  implicit val eqFCallArguments: Eq[FCallArguments] = Eq.instance((args1, args2) =>
    args1.named === args2.named && args1.positional === args2.positional
  )
  implicit val eqFArgument: Eq[FArgument] = Eq.instance({
    case (PositionalArgument(v1), PositionalArgument(v2)) => v1 === v2
    case (NamedArgument(n1, v1), NamedArgument(n2, v2)) =>
      n1 === n2 && v1 === v2
    case _ => false
  })
  implicit val eqNamedArgument: Eq[NamedArgument] =
    Eq.instance((arg1, arg2) => arg1 === arg2)
}
