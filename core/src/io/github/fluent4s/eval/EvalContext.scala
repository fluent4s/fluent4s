package io.github.fluent4s.eval

import io.github.fluent4s.api.FluentValue

import java.util.Locale

case class EvalContext(locale: Locale, args: Map[String, FluentValue]) {

  def get(arg: String): Option[FluentValue] = args.get(arg)
}
