package io.github.fluent4s

import cats.data.ValidatedNel
import io.github.fluent4s.api.{FluentValue, TranslationError}

package object eval extends EvalBase
  with EvalEntry
  with EvalInlineExpression
  with EvalPattern {

  implicit class InfixEvaluator[A](value: A)(implicit evaluator: Evaluator[A]) {

    def evaluate(key: String)(implicit context: EvalContext): ValidatedNel[TranslationError, FluentValue] = evaluator.evaluate(value, key)
  }
}
