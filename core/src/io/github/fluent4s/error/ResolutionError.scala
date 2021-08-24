package io.github.fluent4s.error

case class ResolutionError(message: String) extends Error

object ResolutionError {

  def NotFound(value: String): ResolutionError = ResolutionError(s"Not found: $value")
}
