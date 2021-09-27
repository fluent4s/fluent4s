package io.github.fluent4s.api

import cats.data.ValidatedNel
import io.github.fluent4s.eval._
import io.github.fluent4s.ir.RResource

import java.util.Locale

class FluentResource(val locale: Locale, ir: RResource) {

  /**
   * Get the message represented by the passed key and evaluated using the given arguments.
   * @param key the message key
   * @param args a Map containing the arguments used for evaluation
   * @return the evaluated message as String or TranslationErrors
   */
  def getMessageWith(key: String)(args: Map[String, FluentValue]): ValidatedNel[TranslationError, String] =
    ir.evaluate(key)(EvalContext(locale, args)).map(_.asString)

  /**
   * Get the message represented by the passed key and evaluated using the given arguments.
   * @param key the message key
   * @param args the arguments used for evaluation as tuples of key -> value
   * @return the evaluated message as String or TranslationErrors
   */
  def getMessage(key: String)(args: (String, FluentValue)*): ValidatedNel[TranslationError, String] =
    getMessageWith(key)(args.toMap)
}
