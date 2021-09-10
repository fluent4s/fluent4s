package io.github.fluent4s.api

trait FluentArgument[A] {

  def asFluent(value: A): FluentValue
}
