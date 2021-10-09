package io.github.fluent4s.api.function

import cats.implicits._
import com.ibm.icu.text.DateFormat
import io.github.fluent4s.api.FluentValue._
import io.github.fluent4s.api._
import io.github.fluent4s.util.{DateField, DateSkeleton, DateStyle}

import java.util.{Date, Locale}

object DateTimeFunction extends FluentFunction {

  override def execute(locale: Locale, positional: List[FluentValue], named: Map[String, FluentValue]): Translation =
    (positional, named) match {

      case (List(Number.Integer(time)), params: Map[String, FluentValue]) if params.nonEmpty =>
        params
          .map { case (name, length) =>
            (
              DateField.fromName(name),
              DateField.Length.fromName(length.asString)
              ).mapN(DateSkeleton.apply)
          }
          .toList
          .combineAll
          .andThen(_.asString)
          .map(DateFormat.getInstanceForSkeleton(_, locale))
          .map(_.format(new Date(time)))
          .map(FluentValue.Text.apply)


      case (List(Number.Integer(time), Text(dateStyle)), _) =>
        DateStyle.fromName(dateStyle).map { style =>
          Text(DateFormat.getDateInstance(style.id, locale)
            .format(new Date(time)))
        }

      case _ =>
        Text(DateFormat.getDateInstance(DateFormat.DEFAULT, locale)
          .format(new Date()))
          .validNel
    }
}
