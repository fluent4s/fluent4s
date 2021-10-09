package io.github.fluent4s.util

import cats.implicits._
import io.github.fluent4s.api.{Evaluation, TranslationError}

import scala.util.Try

object DateStyle extends Enumeration {

  val Full, Long, Medium, Short = Value

  def fromName(name: String): Evaluation[DateStyle] =
    values
      .find(_.toString equalsIgnoreCase name)
      .toValidNel(TranslationError.NotFound(s"Style $name"))
}
