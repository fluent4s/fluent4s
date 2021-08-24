package io.github.fluent4s

import ast._
import cats.data.ValidatedNel
import io.github.fluent4s.error.ResolutionError

package object rst extends ResolvedBase
  with ResolvedBlockExpression
  with ResolvedEntry
  with ResolvedInlineExpression
  with ResolvedPattern