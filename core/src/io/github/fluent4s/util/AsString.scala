package io.github.fluent4s.util

import io.github.fluent4s.api.Evaluation

trait AsString[-T] {

  def asString(input: T): Evaluation[String]
}
