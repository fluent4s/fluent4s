package io.github.fluent4s.api

import cats.data.ValidatedNel
import io.github.fluent4s.api.function.DateTimeFunction

import java.util.Locale

trait FluentFunction {

  def execute(locale: Locale, positional: List[FluentValue], named: Map[String, FluentValue]): Translation
}

object FluentFunction {

  val BuiltIn: Map[String, FluentFunction] = Map(
    "DATETIME" -> DateTimeFunction
  )
}