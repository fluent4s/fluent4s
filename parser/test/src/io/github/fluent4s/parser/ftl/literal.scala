
package io.github.fluent4s.parser.ftl

import cats.implicits.toShow
import io.github.fluent4s.ast._
import io.github.fluent4s.parser.{UnitSpec, Ftl}
import org.scalatest.matchers.should._
import scala.collection.mutable

class FtlLiteralSpec extends UnitSpec {
    private val r = util.Random
    
  /*
   * unicode_excape parser related tests.
   */
  "unicode_escape parser" should "parse 2-bytes Unicode code-points" in {
    assert(Ftl.unicode_escape.parseAll("\\u0020") == Right("\u0020"))
    assert(Ftl.unicode_escape.parseAll("\\u0810") == Right("\u0810"))
    assert(Ftl.unicode_escape.parseAll("\\u0200") == Right("\u0200"))
    assert(Ftl.unicode_escape.parseAll("\\uffff") == Right("\uffff"))
  }
  it should "parse 3-bytes Unicode code-points" in {
    assert(Ftl.unicode_escape.parseAll("\\U000020") == Right("\u0020"))
    assert(Ftl.unicode_escape.parseAll("\\U000810") == Right("\u0810"))
    assert(Ftl.unicode_escape.parseAll("\\U000200") == Right("\u0200"))
    assert(Ftl.unicode_escape.parseAll("\\U00ffff") == Right("\uffff"))
    assert(Ftl.unicode_escape.parseAll("\\U010000") == Right("𐀀"))
    assert(Ftl.unicode_escape.parseAll("\\U01F910") == Right("🤐"))
  }

  /*
   * quoted_char parser related tests.
   */
  "quoted_char parser" should "parse escaped char (like \\ or \")" in {
    withClue("parsing antislash") {
      assert(Ftl.quoted_char.parseAll("\\\\") == Right("\\"))
    }
    withClue("parsing double quote") {
      assert(Ftl.quoted_char.parseAll("\\\"") == Right("\""))
    }
  }

  it should "parse Unicode code-points" in {
    withClue("parsing 2-bytes Unicode code-points") {
      assert(Ftl.quoted_char.parseAll("\\u0020") == Right("\u0020"))
      assert(Ftl.quoted_char.parseAll("\\u0810") == Right("\u0810"))
      assert(Ftl.quoted_char.parseAll("\\u0200") == Right("\u0200"))
      assert(Ftl.quoted_char.parseAll("\\uffff") == Right("\uffff"))
    }
    withClue("parsing 3-bytes Unicode code-points") {
      assert(Ftl.quoted_char.parseAll("\\U00ffff") == Right("\uffff"))
      assert(Ftl.quoted_char.parseAll("\\U01F910") == Right("🤐"))
    }
  }

  it should "parse alphanumeric chars" in {
    withClue("parsing digit") {
      for (i <- '0' to '9') assert(Ftl.quoted_char.parseAll(i.toString).isRight)
    }
    withClue("parsing alpha") {
      for (i <- 'a' to 'z') assert(Ftl.quoted_char.parseAll(i.toString).isRight)
    }
    withClue("parsing alpha") {
      for (i <- 'A' to 'Z') assert(Ftl.quoted_char.parseAll(i.toString).isRight)
    }
  }


  /*
   * text_char parser related tests.
   */
  "text_char parser" should "not parse on EOF and placeable delimiter" in {
    for (i <- List("{", "}", ""))
      assert(Ftl.text_char.parseAll(i.toString).isLeft)
  }
  it should "parse alphanumeric chars" in {
    withClue("parsing digit") {
      for (i <- '0' to '9') assert(Ftl.text_char.parseAll(i.toString).isRight)
    }
    withClue("parsing alpha") {
      for (i <- 'a' to 'z') assert(Ftl.text_char.parseAll(i.toString).isRight)
    }
    withClue("parsing alpha") {
      for (i <- 'A' to 'Z') assert(Ftl.text_char.parseAll(i.toString).isRight)
    }
  }

  /*
   * indented_char parser related tests.
   */
  "indented_char parser" should "not parse on indented char used by variant and attribute" in {
    for (i <- List("[", ".", "*"))
      assert(Ftl.indented_char.parseAll(i.toString).isLeft)
  }

  it should "neither parse on EOF and placeable delimiter" in {
    for (i <- List("{", "}", ""))
      assert(Ftl.indented_char.parseAll(i.toString).isLeft)
  }

  it should "parse alphanumeric chars" in {
    withClue("parsing digit") {
      for (i <- '0' to '9')
        assert(Ftl.indented_char.parseAll(i.toString).isRight)
    }
    withClue("parsing alpha") {
      for (i <- 'a' to 'z')
        assert(Ftl.indented_char.parseAll(i.toString).isRight)
    }
    withClue("parsing alpha") {
      for (i <- 'A' to 'Z')
        assert(Ftl.indented_char.parseAll(i.toString).isRight)
    }
  }

  /*
   * string_literal parser related tests.
   */
  "string_literal parser" should "parse escaped char (like \\ or \")" in {
    withClue("parsing antislash") {
      assert(
        Ftl.string_literal.parseAll("\"just some word\\\\\"") == Right(
          "just some word\\"
        )
      )
    }
    withClue("parsing double quote") {
      assert(
        Ftl.string_literal.parseAll("\"to test\\\" if it works\"") == Right(
          "to test\" if it works"
        )
      )
    }
  }

  it should "parse Unicode code-points" in {
    withClue("parsing 2-bytes Unicode code-points") {
      assert(
        Ftl.string_literal.parseAll("\"Hey\\u0020alright?\"") == Right(
          "Hey alright?"
        )
      )
      assert(
        Ftl.string_literal.parseAll("\"something\\u0810is there\"") == Right(
          "something\u0810is there"
        )
      )
      assert(Ftl.string_literal.parseAll("\"\\u0200\"") == Right("\u0200"))
      assert(
        Ftl.string_literal.parseAll("\"\\uffff dema\"") == Right("\uffff dema")
      )
    }
    withClue("parsing 3-bytes Unicode code-points") {
      assert(Ftl.string_literal.parseAll("\"\\U00ffff\"") == Right("\uffff"))
      assert(Ftl.string_literal.parseAll("\"Try\\U01F910\"") == Right("Try🤐"))
    }
  }

  it should "parse full string" in {
    withClue("parsing digit") {
      for (i <- 0 to 10) {
        val digits = r.nextInt.toString
        withClue(s"parsing \"$digits\"") {
          assert(
            Ftl.string_literal.parseAll(s"\"$digits\"") == Right(digits)
          )
        }
      }
    }
    withClue("parsing string") {
      for (i <- 0 to 10) {
        val value = r.nextString(r.nextInt(400))
        withClue(s"parsing \"$value\"") {

          assert(Ftl.string_literal.parseAll(s"\"$value\"") == Right(value))
        }
      }
    }
  }

  /*
   * number_literal parser related tests.
   */
  "number_literal parser" should "parse signed and unsigned integers" in {
    withClue("parsing unsigned number") {
      for (i <- 0 to 10) {
        val digits = r.nextInt(Int.MaxValue).toString
        withClue(s"parsing $digits") {
          assert(
            Ftl.number_literal.parseAll(s"$digits") == Right(digits)
          )
        }
      }
    }
    withClue("parsing signed number") {
      for (i <- 0 to 10) {
        val digits = r.nextInt(Int.MaxValue).toString
        withClue(s"parsing -$digits") {
          assert(
            Ftl.number_literal.parseAll(s"-$digits") == Right(s"-$digits")
          )
        }
      }
    }
  }

  it should "parse signed and unsigned float" in {
    withClue("parsing unsigned number") {
      for (i <- 0 to 10) {
        val digits = r.nextFloat.toString
        withClue(s"parsing $digits") {
          assert(
            Ftl.number_literal.parseAll(s"$digits") == Right(digits)
          )
        }
      }
    }
    withClue("parsing signed number") {
      for (i <- 0 to 10) {
        val digits = r.nextFloat.toString
        withClue(s"parsing -$digits") {
          assert(
            Ftl.number_literal.parseAll(s"-$digits") == Right(s"-$digits")
          )
        }
      }
    }
  }
}