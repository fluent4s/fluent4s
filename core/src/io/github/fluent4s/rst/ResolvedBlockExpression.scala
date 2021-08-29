package io.github.fluent4s.rst

import cats.data.ValidatedNel
import cats.implicits._
import io.github.fluent4s.ast._
import io.github.fluent4s.error.ResolutionError
import net.xyzsd.plurals.PluralCategory

import scala.jdk.OptionConverters.RichOptional

trait ResolvedBlockExpression {

  case class RVariant(key: RVariantKey, value: RPattern, default: Boolean)

  sealed class RVariantKey

  case class RIdentifierKey(value: PluralCategory) extends RVariantKey

  case class RNumberLiteralKey(value: Double) extends RVariantKey

  implicit object VariantResolver extends Resolver[FVariant, RVariant] {

    override def resolve(input: FVariant)(context: Context): Resolution[RVariant] = (
      VariantKeyResolver.resolve(input.key)(context),
      PatternResolver.resolve(input.value)(context),
      input.default.validNel
    ).mapN(RVariant.apply)
  }

  implicit object VariantKeyResolver extends Resolver[FVariantKey, RVariantKey] {

    override def resolve(input: FVariantKey)(context: Context): Resolution[RVariantKey] = input match {

      case IdentifierKey(value) =>
        PluralCategory.ifPresentIgnoreCase(value.name)
          .toScala
          .toValidNel(ResolutionError.Mismatch("PluralCategory", value.name))
          .map(RIdentifierKey.apply)
      case NumberLiteralKey(value) =>
        value
        .toDoubleOption
        .toValidNel(ResolutionError(s"Number expected, got $value"))
        .map(RNumberLiteralKey.apply)
    }
  }
}
