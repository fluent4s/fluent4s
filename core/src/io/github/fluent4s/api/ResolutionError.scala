package io.github.fluent4s.api

case class ResolutionError(message: String) extends Error

object ResolutionError {

  def NotFound(value: String): ResolutionError = ResolutionError(s"Not found: $value")

  def Mismatch(expected: String, got: String): ResolutionError = ResolutionError(s"Expected $expected, got $got")

  val Impossible: ResolutionError = ResolutionError("Impossible error. This is probably a bug.")
}
