package io.github.fluent4s.eval

import cats.data.ValidatedNel
import io.github.fluent4s.api.{FluentValue, TranslationError}

trait Evaluator[-A] {

  /**
   * Evaluate the input and looks for the term identified by the given key.
   * @param input the IR to evaluate
   * @param key the key to search for (for example, a message id)
   * @param context data required by the evaluation process (for example, user arguments)
   * @return the evaluated FluentValue or evaluation errors
   */
  def evaluate(input: A, key: String)(implicit context: EvalContext): Translation
}