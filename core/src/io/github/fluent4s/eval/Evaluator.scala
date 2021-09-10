package io.github.fluent4s.eval

import cats.data.ValidatedNel
import io.github.fluent4s.api.{FluentValue, TranslationError}

trait Evaluator[-A] {

  def evaluate(input: A, key: String)(implicit context: EvalContext): ValidatedNel[TranslationError, FluentValue]
}