package io.github.fluent4s.api

/**
 * Represent a syntax error during the parsing process.
 * @param cursor the location of this error
 * @param message the description of this error
 */
case class ParsingError(cursor: Int, message: String) extends Error(message)