package io.github.fluent4s.parser.ftl

import io.github.fluent4s.parser.{UnitSpec, Ftl};
import cats.implicits.toShow
import io.github.fluent4s.parser.ast._
import org.scalatest.matchers.should._
import scala.collection.mutable

class FtlMiscellaneousSpec extends UnitSpec {
  private val r = util.Random

  /*
   * blank_inline parser related tests.
   */
  "blank_inline parser" should "not parse an arbitrary number of whitespace" in {
    for {
      n <- 1 to 50
    } assert(Ftl.blank_inline.parseAll(" " * n).isRight)
  }
  it should "produce an error when trying to parse an empty string" in {
    assert(Ftl.blank_inline.parseAll("").isLeft)
  }

  /*
   * line_end parser related tests.
   */
  "line_end parser" should "support LF ending" in {
    assert(Ftl.line_end.parseAll("\n").isRight)
  }
  it should "support CRLF ending" in {
    assert(Ftl.line_end.parseAll("\r\n").isRight)
  }
  it should "not support CR ending" in {
    assert(Ftl.line_end.parseAll("\r").isLeft)
  }

  /*
   * blank_block parser related tests.
   */
  "blank_block parser" should "support an arbitrary number of whitespace without line_end" in {
    for {
      n <- 1 to 50
    } assert(Ftl.blank_block.parseAll(" " * n).isLeft)
  }
  it should "support an arbitrary number of newline" in {
    for {
      n <- 1 to 50
    } {
      withClue("using (" + n + ") CRLF ending") {
        assert(Ftl.blank_block.parseAll("\r\n" * n).isRight)
      }
      withClue("using (" + n + ") LF ending") {
        assert(Ftl.blank_block.parseAll("\n" * n).isRight)
      }
    }
  }
  it should "support an arbitrary number of both newline and whitespace" in {
    for {
      i <- 1 to 50
      j <- 1 to 50
    } {
      withClue("using (" + i + ") CRLF ending & (" + j + ") spaces") {
        assert(Ftl.blank_block.parseAll(("\r\n" * i) + (" " * j) + "\r\n").isRight)
      }
      withClue("using (" + i + ") LF ending & (" + j + ") spaces") {
        assert(Ftl.blank_block.parseAll(("\n" * i) + (" " * j) + "\n").isRight)
      }
      val r = scala.util.Random
      withClue("using random number of both") {
        var str = new mutable.StringBuilder()
        for {
          i <- 1 to 50
        } {
          r.nextInt(2) match {
            case 0 => str ++= " " * (r.nextInt(100) + 1)
            case 1 => str ++= "\n" * (r.nextInt(100) + 1)
            case 2 => str ++= "\r\n" * (r.nextInt(100) + 1)
          }
        }
        assert(Ftl.blank_block.parseAll(s"$str\n").isRight)
      }
    }
  }

  /*
   * blank parser related tests.
   */
  "blank parser" should "support an empty string" in {
    assert(Ftl.blank.parseAll("").isRight)
  }
  it should "support an arbitrary number of whitespace" in {
    for {
      n <- 1 to 50
    } assert(Ftl.blank.parseAll(" " * n).isRight)
  }
  it should "support an arbitrary number of newline" in {
    for {
      n <- 1 to 50
    } {
      withClue("using (" + n + ") CRLF ending") {
        assert(Ftl.blank.parseAll("\r\n" * n).isRight)
      }
      withClue("using (" + n + ") LF ending") {
        assert(Ftl.blank.parseAll("\n" * n).isRight)
      }
    }
  }
  it should "support an arbitrary number of both newline and whitespace" in {
    for {
      i <- 1 to 50
      j <- 1 to 50
    } {
      withClue("using (" + i + ") CRLF ending & (" + j + ") spaces") {
        assert(Ftl.blank.parseAll(("\r\n" * i) + (" " * j)).isRight)
      }
      withClue("using (" + i + ") LF ending & (" + j + ") spaces") {
        assert(Ftl.blank.parseAll(("\n" * i) + (" " * j)).isRight)
      }
      val r = scala.util.Random
      withClue("using random of both") {
        var str = new mutable.StringBuilder()
        for {
          i <- 1 to 50
        } {
          r.nextInt(2) match {
            case 0 => str ++= " " * r.nextInt(100)
            case 1 => str ++= "\n" * r.nextInt(100)
            case 2 => str ++= "\r\n" * r.nextInt(100)
          }
        }
        assert(Ftl.blank.parseAll(str.toString).isRight)
      }
    }
  }
}
