package io.github.fluent4s.eval

import io.github.fluent4s.api.FluentValue

import java.util.Locale

/**
 * A context used by the evaluation process.
 * @param locale the locale of the evaluated tree
 * @param args the user-passed arguments (used for variables)
 */
case class EvalContext(locale: Locale, args: Map[String, FluentValue]) {

  def get(arg: String): Option[FluentValue] = args.get(arg)
}
