package io.github.fluent4s.api

import cats.data.ValidatedNel
import io.github.fluent4s.eval._
import io.github.fluent4s.ir.RResource

import java.util.Locale

class FluentResource(val locale: Locale, ir: RResource) {

  def getMessageWith(key: String)(args: Map[String, FluentValue]): ValidatedNel[TranslationError, String] =
    ir.evaluate(key)(EvalContext(locale, args)).map(_.asString)

  def getMessage(key: String)(args: (String, FluentValue)*): ValidatedNel[TranslationError, String] =
    getMessageWith(key)(args.toMap)
}
