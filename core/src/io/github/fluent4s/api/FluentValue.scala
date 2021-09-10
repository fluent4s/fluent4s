package io.github.fluent4s.api

import cats.Monoid

sealed trait FluentValue {

  def asString: String
}

object FluentValue {

  case class Text(value: String) extends FluentValue {
    override def asString: String = value
  }

  case class Number(value: Double) extends FluentValue {
    override def asString: String = value.toString
  }

  case object Empty extends FluentValue {
    override def asString: String = ""
  }

  implicit object ValueMonoid extends Monoid[FluentValue] {
    override def empty: FluentValue = Empty

    override def combine(x: FluentValue, y: FluentValue): FluentValue = Text(x.asString + y.asString)
  }
}