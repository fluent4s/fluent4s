package io.github.fluent4s.rst

import cats.data.ValidatedNel
import cats.implicits._
import io.github.fluent4s.ast._
import io.github.fluent4s.error.ResolutionError

trait ResolvedPattern {

  type RPattern = List[RPatternElement]

  sealed class RPatternElement

  case class RTextElement(value: String) extends RPatternElement

  case class RPlaceable(expression: RExpression) extends RPatternElement

  implicit object PatternResolver extends Resolver[FPattern, RPattern] {

    override def resolve(input: FPattern)(context: Context): Resolution[RPattern] =
      input
        .elements
        .map(_.resolve(context))
        .sequence
  }

  implicit object PatternElementResolver extends Resolver[FPatternElement, RPatternElement] {

    override def resolve(input: FPatternElement)(context: Context): Resolution[RPatternElement] = input match {

      case TextElement(value) => RTextElement(value).validNel

      case BlockTextElement(_, value) => RTextElement(s"\n$value").validNel

      case Placeable(expression) => expression.resolve(context).map(RPlaceable.apply)
    }
  }
}
