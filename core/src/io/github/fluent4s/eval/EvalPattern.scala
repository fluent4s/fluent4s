package io.github.fluent4s.eval

import cats.data.ValidatedNel
import cats.implicits._
import io.github.fluent4s.api.FluentValue._
import io.github.fluent4s.api.{FluentValue, TranslationError}
import io.github.fluent4s.ir._

trait EvalPattern {

  implicit object PatternEvaluator extends Evaluator[RPattern] {
    override def evaluate(input: RPattern, key: String)(implicit context: EvalContext): Translation =
      input.foldMap(_.evaluate(key))
  }

  implicit object PatternElementEvaluator extends Evaluator[RPatternElement] {
    override def evaluate(input: RPatternElement, key: String)(implicit context: EvalContext): Translation = input match {

      case RTextElement(value) => Text(value).validNel
      case RPlaceable(expression) => expression.evaluate(key)
    }
  }
}
