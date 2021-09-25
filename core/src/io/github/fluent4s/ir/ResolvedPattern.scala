package io.github.fluent4s.ir

import cats.data.ValidatedNel
import cats.implicits._
import io.github.fluent4s.api.ResolutionError
import io.github.fluent4s.ast._

trait ResolvedPattern {

  type RPattern = List[RPatternElement]

  sealed trait RPatternElement

  case class RTextElement(value: String) extends RPatternElement

  case class RPlaceable(expression: RExpression) extends RPatternElement

  implicit object PatternResolver extends Resolver[FPattern, RPattern] {

    override def resolve(input: FPattern)(implicit context: ResolutionContext): Resolution[RPattern] =
      input
        .elements
        .map(_.resolve(context))
        .sequence
  }

  implicit object PatternElementResolver extends Resolver[FPatternElement, RPatternElement] {

    override def resolve(input: FPatternElement)(implicit context: ResolutionContext): Resolution[RPatternElement] = input match {

      case TextElement(value) => RTextElement(value).validNel

      case BlockTextElement(_, value) => RTextElement(s"\n$value").validNel

      case Placeable(expression) => expression.resolve(context).map(RPlaceable.apply)
    }
  }
}
