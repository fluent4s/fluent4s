package io.github.fluent4s

import io.github.fluent4s.api.FluentValue.{Number, Text}

package object api {

  implicit object StringArgument extends FluentArgument[String] {
    override def asFluent(value: String): FluentValue = Text(value)
  }

  implicit object IntegerArgument extends FluentArgument[Long] {
    override def asFluent(value: Long): FluentValue = Number(value)
  }

  implicit object DecimalArgument extends FluentArgument[Double] {
    override def asFluent(value: Double): FluentValue = Number(value)
  }
}
