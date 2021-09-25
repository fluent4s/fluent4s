package io.github.fluent4s.api

import io.github.fluent4s.ast.FResource

trait FluentParser {

  /**
   * Parse the given text into a FResource
   * @param text the fluent code to be parsed
   * @return the AST parsed from the given text
   */
  def parse(text: String): Either[Error, FResource]
}
