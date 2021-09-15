package io.github.fluent4s.ir

import cats.implicits._
import io.github.fluent4s.api.ResolutionError
import io.github.fluent4s.ast._

trait ResolvedBlockExpression {

  case class RVariant(key: RVariantKey, value: RPattern, default: Boolean)

  sealed trait RVariantKey

  case class RWordKey(value: String) extends RVariantKey

  case class RPluralKey(value: String) extends RVariantKey

  case class RIntegerLiteralKey(value: Long) extends RVariantKey

  case class RDecimalLiteralKey(value: Double) extends RVariantKey

  implicit object VariantResolver extends Resolver[FVariant, RVariant] {

    override def resolve(input: FVariant)(implicit context: Context): Resolution[RVariant] = (
      VariantKeyResolver.resolve(input.key),
      PatternResolver.resolve(input.value),
      input.default.validNel
      ).mapN(RVariant.apply)
  }

  implicit object VariantKeyResolver extends Resolver[FVariantKey, RVariantKey] {

    override def resolve(input: FVariantKey)(implicit context: Context): Resolution[RVariantKey] = input match {

      case IdentifierKey(FIdentifier(name)) =>
        if (context.pluralRules.getKeywords.contains(name)) RPluralKey(name).validNel
        else RWordKey(name).validNel

      case IntegerLiteralKey(value) => RIntegerLiteralKey(value).validNel

      case DecimalLiteralKey(value) => RDecimalLiteralKey(value).validNel
    }
  }
}
