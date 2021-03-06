package io.github.fluent4s.ir

import cats.implicits._
import io.github.fluent4s.api.ResolutionError
import io.github.fluent4s.ast._
import io.github.fluent4s.util._

trait ResolvedBase {

  /**
   * Represent the root of the Intermediate Representation
   * @param body a Map representing the pair id -> resolved node
   */
  case class RResource(body: Map[String, REntry])

  type RAttribute = (String, RPattern)

  sealed trait RExpression

  case class RSelect(selector: RInlineExpression, variants: List[RVariant]) extends RExpression

  case class RInline(body: RInlineExpression) extends RExpression

  implicit object ResourceResolver extends Resolver[FResource, RResource] {

    override def resolve(input: FResource)(implicit context: ResolutionContext): Resolution[RResource] = {
      input
        .body
        .filterType[ReferenceEntry]
        .foldLeft(context.validNel[ResolutionError])((ctxResolution: Resolution[ResolutionContext], entry: ReferenceEntry) => (
            ctxResolution,
            ctxResolution.andThen(entry.resolve(_))
            ).mapN(_.withReference(entry.body.id.name, _))
        )
        .map(ctx => RResource(ctx.references))
    }
  }

  implicit object AttributeResolver extends Resolver[FAttribute, RAttribute] {

    override def resolve(input: FAttribute)(implicit context: ResolutionContext): Resolution[(String, RPattern)] =
      input.value.resolve.map((input.id.name, _))
  }

  implicit object ExpressionResolver extends Resolver[FExpression, RExpression] {

    override def resolve(input: FExpression)(implicit context: ResolutionContext): Resolution[RExpression] = input match {

      case Select(selector, variants) => (
        selector.resolve(context),
        variants.map(_.resolve).sequence
        ).mapN(RSelect.apply)

      case Inline(body) => body.resolve.map(RInline.apply)
    }
  }
}