package io.github.fluent4s

import cats.data.ValidatedNel
import cats.implicits._
import io.github.fluent4s.ir._

import java.util.Locale


package object api {

  type Evaluation[T] = ValidatedNel[TranslationError, T]
  type Translation = Evaluation[FluentValue]

  /**
   * Parse the given text into an intermediate representation.
   * @param text the fluent code to be parsed
   * @param locale the locale of this FluentResource
   * @param parser the parser used to generate the AST
   * @return a ValidatedNel containing the parsed resource or a list of parsing/resolution errors
   */
  def decode(text: String, locale: Locale, functions: Map[String, FluentFunction] = Map.empty)(implicit parser: FluentParser): ValidatedNel[Error, FluentResource] =
    parser
      .parse(text)
      .toValidatedNel
      .andThen(_.resolve(ResolutionContext(locale, Map.empty, FluentFunction.BuiltIn ++ functions)))
      .map(new FluentResource(locale, _))
}
