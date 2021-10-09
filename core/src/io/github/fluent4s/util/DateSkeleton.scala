package io.github.fluent4s.util

import cats.{Monoid, Reducible, Semigroup}
import cats.implicits._
import io.github.fluent4s.api.{Evaluation, TranslationError}
import io.github.fluent4s.util.DateSkeleton.{Concat, Part}

sealed trait DateSkeleton {

  def withPart(field: DateField, length: DateField.Length): DateSkeleton = Concat(this, Part(field, length))
}

object DateSkeleton {

  def apply(field: DateField, length: DateField.Length): DateSkeleton = Part(field, length)

  case object Empty extends DateSkeleton

  case class Part(field: DateField, length: DateField.Length) extends DateSkeleton

  case class Concat(a: DateSkeleton, b: DateSkeleton) extends DateSkeleton

  implicit object SkeletonShow extends AsString[DateSkeleton] {
    override def asString(input: DateSkeleton): Evaluation[String] = input match {

      case Empty => "".validNel

      case Part(field, length) =>
        field
          .formats
          .get(length)
          .map { x =>
            val builder = new StringBuilder
            for (_ <- 0 until x) builder.append(field.pattern)
            builder.toString
          }
          .toValidNel(TranslationError.NotFound(s"Length $length for ${field.pattern} "))

      case Concat(a, b) => asString(a).combine(asString(b))
    }
  }

  implicit object SkeletonReducible extends Monoid[DateSkeleton] {
    override def combine(x: DateSkeleton, y: DateSkeleton): DateSkeleton = Concat(x, y)

    override def empty: DateSkeleton = Empty
  }
}