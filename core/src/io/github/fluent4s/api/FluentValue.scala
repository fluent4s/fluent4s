package io.github.fluent4s.api

import cats.Monoid

/**
 * Represent a valid ProjectFluent value. Can be either text or number.
 */
sealed trait FluentValue {

  def asString: String
}

object FluentValue {

  case class Text(asString: String) extends FluentValue

  /**
   * Represent a valid numerical ProjectFluent value.
   */
  sealed trait Number extends FluentValue {

    def asDouble: Double
  }

  object Number {

    def apply(value: Long): Number = Integer(value)

    def apply(value: Double): Number = Decimal(value)

    case class Integer(value: Long) extends Number {
      override def asDouble: Double = value.toDouble

      override def asString: String = value.toString
    }

    case class Decimal(value: Double) extends Number {
      override def asDouble: Double = value

      override def asString: String = value.toString
    }

    def unapply(arg: Number): Option[Double] = Some(arg.asDouble)
  }

  case object Empty extends FluentValue {
    override def asString: String = ""
  }

  implicit object ValueMonoid extends Monoid[FluentValue] {
    override def empty: FluentValue = Empty

    override def combine(x: FluentValue, y: FluentValue): FluentValue = Text(x.asString + y.asString)
  }
}