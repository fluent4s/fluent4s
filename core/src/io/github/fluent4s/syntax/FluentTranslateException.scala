package io.github.fluent4s.syntax

sealed trait FluentTranslateException extends Exception

object FluentTranslateException {

  class MissingValueException()
}