package io.github.fluent4s.rst

import cats.implicits._
import io.github.fluent4s.ast._
import io.github.fluent4s.error.ResolutionError
import io.github.fluent4s.util._

trait ResolvedBase {

  case class RResource(body: Map[String, REntry])

  type RAttribute = (String, RPattern)

  sealed class RExpression

  case class RSelect(selector: RInlineExpression, variants: List[RVariant]) extends RExpression

  case class RInline(body: RInlineExpression) extends RExpression

  implicit object ResourceResolver extends Resolver[FResource, RResource] {

    override def resolve(input: FResource)(context: Context): Resolution[RResource] = {
      input
        .body
        .filterType[ReferenceEntry]
        .foldLeft(context.validNel[ResolutionError])((ctxResolution: Resolution[Context], entry: ReferenceEntry) => (
            ctxResolution,
            ctxResolution.andThen(entry.resolve)
            ).mapN(_.withReference(entry.body.id.name, _))
        )
        .map(ctx => RResource(ctx.references))
    }
  }

  implicit object AttributeResolver extends Resolver[FAttribute, RAttribute] {

    override def resolve(input: FAttribute)(context: Context): Resolution[(String, RPattern)] =
      input.value.resolve(context).map((input.id.name, _))
  }

  implicit object ExpressionResolver extends Resolver[FExpression, RExpression] {

    override def resolve(input: FExpression)(context: Context): Resolution[RExpression] = input match {

      case Select(selector, variants) => (
        selector.resolve(context),
        variants.map(_.resolve(context)).sequence
        ).mapN(RSelect.apply)

      case Inline(body) => body.resolve(context).map(RInline.apply)
    }
  }
}