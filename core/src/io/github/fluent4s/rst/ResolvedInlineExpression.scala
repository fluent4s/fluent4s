package io.github.fluent4s.rst

import cats.implicits._
import io.github.fluent4s.api.ResolutionError
import io.github.fluent4s.ast._

trait ResolvedInlineExpression {

  sealed class RInlineExpression

  case class RStringLiteral(value: String) extends RInlineExpression

  case class RNumberLiteral(value: Double) extends RInlineExpression

  case class RMessageReference(resolved: RMessage) extends RInlineExpression

  case class RAttributeReference(resolved: RPattern) extends RInlineExpression

  case class RTermReference(resolved: RTerm, arguments: Option[RCallArguments]) extends RInlineExpression

  case class RVariableReference(id: String) extends RInlineExpression

  case class RPlaceableExpr(expression: RExpression) extends RInlineExpression

  case class RCallArguments(positional: List[RInlineExpression], named: Map[String, RInlineExpression])

  implicit object InlineResolver extends Resolver[FInlineExpression, RInlineExpression] {

    override def resolve(input: FInlineExpression)(implicit context: Context): Resolution[RInlineExpression] = input match {

      case StringLiteral(value) => RStringLiteral(value).validNel

      case NumberLiteral(value) =>
        value
          .toDoubleOption
          .toValidNel(ResolutionError(s"Number expected, got $value"))
          .map(RNumberLiteral.apply)

      case FunctionReference(id, arguments) => ResolutionError("Not implemented").invalidNel //TODO Resolve FunctionReference

      case MessageReference(id, attribute) => (context.getReference(id.name), attribute) match {

        case (Some(resolved: RMessage), None) => RMessageReference(resolved).validNel

        case (Some(resolved: RMessage), Some(attrId)) =>
          resolved
            .attributes
            .get(attrId.name)
            .map(RAttributeReference.apply)
            .toValidNel(ResolutionError.NotFound(s"attribute ${attrId.name}"))

        case _ => ResolutionError.NotFound(s"message ${id.name}").invalidNel
      }

      case TermReference(id, attribute, arguments) => (context.getReference(id.name), attribute) match {

        case (Some(resolved: RTerm), None) =>
          arguments
            .map(CallArgumentsResolver.resolve(_))
            .sequence
            .map(RTermReference(resolved, _))

        case (Some(resolved: RTerm), Some(attrId)) =>
          resolved
            .attributes
            .get(attrId.name)
            .map(RAttributeReference.apply)
            .toValidNel(ResolutionError.NotFound(s"attribute ${attrId.name}"))

        case _ => ResolutionError.NotFound(s"attribute ${id.name}").invalidNel
      }

      case VariableReference(id) => RVariableReference(id.name).validNel

      case PlaceableExpr(expression) => expression.resolve.map(RPlaceableExpr.apply)

      case _ => ResolutionError.Impossible.invalidNel
    }
  }

  implicit object CallArgumentsResolver extends Resolver[FCallArguments, RCallArguments] {

    override def resolve(input: FCallArguments)(implicit context: Context): Resolution[RCallArguments] = (
      input
        .positional
        .map(_.resolve)
        .sequence,
      input
        .named
        .map(arg => arg.value.resolve.map((arg.name.name, _)))
        .sequence
        .map(_.toMap)
      ).mapN(RCallArguments.apply)

  }
}
