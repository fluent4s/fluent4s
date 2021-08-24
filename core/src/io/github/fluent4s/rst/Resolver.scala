package io.github.fluent4s.rst

import cats.data.ValidatedNel
import io.github.fluent4s.error.ResolutionError

trait Resolver[A, B] {

  def resolve(input: A)(context: Context): ValidatedNel[ResolutionError, B]
}