package io.github.fluent4s.util

import cats.implicits._
import io.github.fluent4s.api.{Evaluation, TranslationError}
import io.github.fluent4s.util.DateField.Length

import scala.util.Try

case class DateField(pattern: Char, formats: Map[Length, Int])

object DateField {

  type Length = Length.Value

  object Length extends Enumeration {

    val Short, Long, Narrow, Numeric = Value

    def fromName(name: String): Evaluation[Length] =
      values
        .find(_.toString equalsIgnoreCase name)
        .toValidNel(TranslationError.NotFound(s"Length $name"))
  }

  def fromName(name: String): Evaluation[DateField] =
    All
      .get(name)
      .toValidNel(TranslationError.NotFound(s"Field $name"))

  private val All: Map[String, DateField] = Map(
    "era" -> DateField('G', Map(
      Length.Short -> 3,
      Length.Long -> 4,
      Length.Narrow -> 5
    )),
    "year" -> DateField('y', Map(
      Length.Numeric -> 4
    )),
    "month" -> DateField('M', Map(
      Length.Short -> 3,
      Length.Long -> 4,
      Length.Narrow -> 5
    )),
    "weekday" -> DateField('E', Map(
      Length.Short -> 3,
      Length.Long -> 4,
      Length.Narrow -> 5,
      Length.Numeric -> 2
    )),
    "day" -> DateField('d', Map(
      Length.Numeric -> 2
    )),
    "hour" -> DateField('H', Map(
      Length.Numeric -> 2
    )),
    "hour12" -> DateField('h', Map(
      Length.Numeric -> 2
    )),
    "minute" -> DateField('m', Map(
      Length.Numeric -> 2
    )),
    "second" -> DateField('s', Map(
      Length.Numeric -> 2
    )),
    "timeZoneName" -> DateField('z', Map(
      Length.Short -> 3,
      Length.Long -> 4
    ))
  )
}
