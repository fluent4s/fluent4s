package io.github.fluent4s.parser.ftl

import cats.implicits.toShow
import io.github.fluent4s.parser.{Ftl, UnitSpec}
import io.github.fluent4s.ast._
import org.scalatest.matchers.should._

import scala.collection.mutable

class FtlPatternSpec extends UnitSpec {

  /*
   * Inline placeable related tests.
   */
  "inline_placeable parser" should "parse a select expression" in {
    Ftl.inline_placeable.parseAll(
      "{$userGender ->\n[male] his stream\n[female] her stream\n*[other] their stream\n}"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(PlaceableExpr(_: Select)) => succeed
      case Right(pars) => fail(s"not a select expression, parsed: $pars")
    }
    Ftl.inline_placeable.parseAll(
      "{ $userGender -> \n[male] his stream\n[female] her stream\n *[other] their stream\n}"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(PlaceableExpr(_: Select)) => succeed
      case Right(pars) => fail(s"not a select expression, parsed: $pars")
    }
  }

  it should "parse an inline expression" in {
    Ftl.inline_placeable.parseAll("{$variable\n }") match {
      case Left(e) => fail(e.toString)
      case Right(PlaceableExpr(_: Inline)) => succeed
      case Right(pars) => fail(s"not an inline expression, parsed: $pars")
    }
  }

  /*
   * Block placeable related tests.
   */
  "block_placeable parser" should "parse a select expression" in {
    Ftl.block_placeable.parseAll(
      "\n\n\n\n\n\n\n\n\n\n { $userGender -> \n[male] his stream\n[female] her stream\n*[other] their stream\n }"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(PlaceableExpr(_: Select)) => succeed
      case Right(pars) => fail(s"not a select expression, parsed: $pars")
    }
  }

  it should "parse an inline expression" in {
    Ftl.block_placeable.parseAll("\n {$variable}") match {
      case Left(e) => fail(e.toString)
      case Right(PlaceableExpr(_: Inline)) => succeed
      case Right(pars) => fail(s"not an inline expression, parsed: $pars")
    }
  }

  /*
   * Pattern element related tests.
   */
  "pattern_element parser" should "parse an inline text" in {
    Ftl.pattern_element.parseAll("just some string") match {
      case Left(e) => fail(e.toString)
      case Right(TextElement(value)) => assert(value === "just some string")
      case Right(pars) => fail(s"not an inline text pattern, parsed: $pars")
    }

    Ftl.pattern_element.parseAll(" ") match {
      case Left(e) => fail(e.toString)
      case Right(TextElement(value)) => assert(value === " ")
      case Right(pars) => fail(s"not an inline text pattern, parsed: $pars")
    }
  }

  it should "parse a block text" in {
    Ftl.pattern_element.parseAll(" \n    as long as each new line is indented") match {
      case Left(e) => fail(e.toString)
      case Right(BlockTextElement(ident, Some(value))) =>
        assert(ident === 4 && value === "as long as each new line is indented")
      case Right(pars) => fail(s"not a block text pattern\nparsed: $pars")
    }
  }

  it should "parse a block placeable" in {
    Ftl.pattern_element.parseAll(" \n\n {$variable\n }") match {
      case Left(e) => fail(e.toString)
      case Right(Placeable(_: Inline)) => succeed
      case Right(pars) => fail(s"not a block placeable\nparsed: $pars")
    }
  }

  it should "parse an inline placeable" in {
    Ftl.pattern_element.parseAll("{$variable}") match {
      case Left(e) => fail(e.toString)
      case Right(Placeable(_: Inline)) => succeed
      case Right(pars) => fail(s"not an inline placeable\nparsed: $pars")
    }
  }

  /*
   * Pattern related tests.
   */
  "pattern parser" should "parse test with multiple placeables per line" in {
    Ftl.pattern.parseAll(
      "{$photoCount ->\n        [one] added a new photo\n*[other] added {$photoCount} new photos\n } to {$userGender ->\n[male] his stream\n[female] her stream\n*[other] their stream\n}."
    ) match {
      case Left(e) => fail(e.toString)
      case Right(pars) => succeed
    }
  }

  it should "not parse a variant definition" in {
    Ftl.pattern.parseAll("\n        [one] added a new photo") match {
      case Left(e) => succeed
      case Right(pars) => fail(s"parsed: ${pars.show}")
    }
  }

  it should "not parse a default variant definition" in {
    Ftl.pattern.parseAll("\n        *[one] added a new photo") match {
      case Left(e) => succeed
      case Right(pars) => fail(s"parsed: ${pars.show}")
    }
  }
}
