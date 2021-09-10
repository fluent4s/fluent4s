package io.github.fluent4s.eval

import cats.data.ValidatedNel
import cats.implicits._
import io.github.fluent4s.api.{FluentValue, TranslationError}
import io.github.fluent4s.ir._

trait EvalEntry {

  implicit object EntryResolver extends Evaluator[REntry] {
    override def evaluate(input: REntry, key: String)(implicit context: EvalContext): ValidatedNel[TranslationError, FluentValue] = input match {

      case RMessage(value, attributes) =>
        value
          .toValidNel(new TranslationError(s"No text for message $input"))
          .andThen(_.evaluate(key))

      case RTerm(value, attributes) => value.evaluate(key)
    }
  }
}
