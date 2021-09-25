package io.github.fluent4s.api

/**
 * Represent an error related to the evaluation process (unknown message, missing argument...).
 * @param message the description of this error
 */
case class TranslationError(message: String) extends Error(message)

object TranslationError {

  def NaN(value: String): TranslationError = new TranslationError(s"Not a number: $value")

  def MissingArgument(key: String): TranslationError = new TranslationError(s"Missing argument: $key")
}