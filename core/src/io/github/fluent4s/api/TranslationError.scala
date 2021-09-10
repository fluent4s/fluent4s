package io.github.fluent4s.api

class TranslationError(message: String) extends Error(message)

object TranslationError {

  def NaN(value: String): TranslationError = new TranslationError(s"Not a number: $value")

  def MissingArgument(key: String): TranslationError = new TranslationError(s"Missing argument: $key")
}