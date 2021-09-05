package io.github.fluent4s.parser

import cats.implicits._

import io.github.fluent4s.ast._

import utest._

object BaseParserSuite extends TestSuite {

  val tests: Tests = Tests {

    test("blank_inline") {
      test("whitespace") {
        assert(Ftl.blank_inline.parseAll(" " * 1).isRight)
        assert(Ftl.blank_inline.parseAll(" " * 5).isRight)
        assert(Ftl.blank_inline.parseAll(" " * 10).isRight)
        assert(Ftl.blank_inline.parseAll(" " * 50).isRight)
        assert(Ftl.blank_inline.parseAll(" " * 255).isRight)
      }
      test("empty_string") {
        assert(Ftl.blank_inline.parseAll("").isLeft)
      }
    }

    test("line_end") {
      test("lf") {
        assert(Ftl.line_end.parseAll("\n").isRight)
      }
      test("crlf") {
        assert(Ftl.line_end.parseAll("\r\n").isRight)
      }
      test("cr") {
        assert(Ftl.line_end.parseAll("\r").isLeft)
      }
      test("multilines") {
        assert(Ftl.line_end.parseAll("\n" * 5).isLeft)
        assert(Ftl.line_end.parseAll("\r\n" * 5).isLeft)
      }
    }

    test("blank_block") {
      test("whitespace") {
        assert(Ftl.blank_block.parseAll(" " * 1).isLeft)
      }
      test("newlines") {
        assert(Ftl.blank_block.parseAll("\n" * 1).isRight)
        assert(Ftl.blank_block.parseAll("\r\n" * 5).isRight)
        assert(Ftl.blank_block.parseAll("\r\n" * 10).isRight)
        assert(Ftl.blank_block.parseAll("\n" * 50).isRight)
        assert(Ftl.blank_block.parseAll("\n" * 255).isRight)
      }
      test("whitespace_and_newlines") {
        assert(Ftl.blank_block.parseAll("   \n").isRight)
        assert(Ftl.blank_block.parseAll("\r\n \n" * 5).isRight)
        assert(Ftl.blank_block.parseAll("    \r\n" * 10).isRight)
        assert(Ftl.blank_block.parseAll("\n    " * 50 + "\n").isRight)
        assert(Ftl.blank_block.parseAll("  \n" * 255).isRight)
      }
    }

    test("blank") {
      test("whitespaces") {
        assert(Ftl.blank.parseAll(" " * 1).isRight)
        assert(Ftl.blank.parseAll(" " * 5).isRight)
        assert(Ftl.blank.parseAll(" " * 10).isRight)
        assert(Ftl.blank.parseAll(" " * 50).isRight)
        assert(Ftl.blank.parseAll(" " * 255).isRight)
      }
      test("newlines") {
        assert(Ftl.blank.parseAll("\n" * 1).isRight)
        assert(Ftl.blank.parseAll("\r\n" * 5).isRight)
        assert(Ftl.blank.parseAll("\r\n" * 10).isRight)
        assert(Ftl.blank.parseAll("\n" * 50).isRight)
        assert(Ftl.blank.parseAll("\n" * 255).isRight)
      }
      test("whitespace_and_newlines") {
        assert(Ftl.blank.parseAll("   \n").isRight)
        assert(Ftl.blank.parseAll("\r\n \n" * 5).isRight)
        assert(Ftl.blank.parseAll("    \r\n" * 10).isRight)
        assert(Ftl.blank.parseAll("\n    " * 50).isRight)
        assert(Ftl.blank.parseAll("  \n" * 255).isRight)
      }
    }

  }
}
