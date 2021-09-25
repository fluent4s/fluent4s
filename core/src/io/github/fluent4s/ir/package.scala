package io.github.fluent4s

import cats.data.ValidatedNel
import io.github.fluent4s.api.ResolutionError

package object ir extends ResolvedBase
  with ResolvedBlockExpression
  with ResolvedEntry
  with ResolvedInlineExpression
  with ResolvedPattern {

  type Resolution[B] = ValidatedNel[ResolutionError, B]

  implicit class Resolvable[A, B](input: A)(implicit resolver: Resolver[A, B]) {

    def resolve(implicit context: ResolutionContext): Resolution[B] = resolver.resolve(input)
  }
}