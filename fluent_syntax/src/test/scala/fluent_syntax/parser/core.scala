package fluent_syntax.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.PrivateMethodTester._
import cats.parse.{Parser, Parser0}
import fluent_syntax.parser.Ftl
import fluent_syntax.ast._
import cats.implicits._
import org.scalatest.matchers.should._
import scala.collection.mutable

class FtlSpec extends AnyFlatSpec with Matchers {
  val r = util.Random

  "blank_inline parser" should "successfully parse an arbitrary number of whitespace" in {
    for {
      n <- 1 to 50
    } assert(Ftl.blank_inline.parseAll(" " * n).isRight)
  }
  it should "produce an error when trying to parse an empty string" in {
    assert(Ftl.blank_inline.parseAll("").isLeft)
  }

  "line_end parser" should "support LF ending" in {
    assert(Ftl.line_end.parseAll("\n").isRight)
  }
  it should "support CRLF ending" in {
    assert(Ftl.line_end.parseAll("\r\n").isRight)
  }
  it should "not support CR ending" in {
    assert(Ftl.line_end.parseAll("\r").isLeft)
  }

  "blank_block parser" should "support an arbitrary number of whitespace" in {
    for {
      n <- 1 to 50
    } assert(Ftl.blank_block.parseAll(" " * n).isRight)
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
        assert(Ftl.blank_block.parseAll(("\r\n" * i) + (" " * j)).isRight)
      }
      withClue("using (" + i + ") LF ending & (" + j + ") spaces") {
        assert(Ftl.blank_block.parseAll(("\n" * i) + (" " * j)).isRight)
      }
      val r = scala.util.Random
      withClue("using random of both") {
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
        assert(Ftl.blank_block.parseAll(str.toString).isRight)
      }
    }
  }

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

  "identifier parser" should "parse an alphanumerical string starting with a letter" in {
    withClue("one letter only") {
      for (i <- 'a' to 'z') {
        Ftl.identifier.parseAll(s"$i") match {
          case Left(e) => fail(e.toString)
          case Right(id) => assert(id.name === i.toString)
        }
      }
      for (i <- 'A' to 'Z') {
        Ftl.identifier.parseAll(s"$i") match {
          case Left(e) => fail(e.toString)
          case Right(id) => assert(id.name === i.toString)
        }
      }
    }
    for (i <- 'a' to 'z') {
      val random_tail = r.alphanumeric.take(r.nextInt(200)).mkString("");
      withClue(s"parsing id: $i$random_tail") {
        Ftl.identifier.parseAll(s"$i$random_tail") match {
          case Left(e) => fail(e.toString)
          case Right(id) => assert(id.name === s"$i$random_tail")
        }
      }
    }
    for (i <- 'A' to 'Z') {
      val random_tail = r.alphanumeric.take(r.nextInt(200)).mkString("_");
      withClue(s"parsing id: $i$random_tail") {
        Ftl.identifier.parseAll(s"$i$random_tail") match {
          case Left(e) => fail(e.toString)
          case Right(id) => assert(id.name === s"$i$random_tail")
        }
      }
    }
    for (i <- 'a' to 'z') {
      val random_tail = r.alphanumeric.take(r.nextInt(200)).mkString("-");
      withClue(s"parsing id: $i$random_tail") {
        Ftl.identifier.parseAll(s"$i$random_tail") match {
          case Left(e) => fail(e.toString)
          case Right(id) => assert(id.name === s"$i$random_tail")
        }
      }
    }
  }

  it should "not parse an identifier starting by a digit" in {

    for (i <- '0' to '9') {
      withClue(s"parsing id: $i") {
        Ftl.identifier.parseAll(s"$i") match {
            case Left(e) => succeed
            case Right(id) => fail(s"parsed: ${id.name}")
        }
      }
      val random_tail = r.alphanumeric.take(r.nextInt(200)).mkString("");
      withClue(s"parsing id: $i$random_tail") {
        Ftl.identifier.parseAll(s"$i$random_tail") match {
            case Left(e) => succeed
            case Right(id) => fail(s"parsed: ${id.name}")
        }
      }
    }
  }

  "variant_key parser" should "parse numeral keys" in {
    for (i <- 0 to 10) {
      withClue("parsing signed/unsigned integers") {
        val value = r.nextInt(200)-100;
        withClue(s"parsing: [$value]") {
          Ftl.variant_key.parseAll(s"[$value]") match {
              case Left(e) => fail(e.toString)
              case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
              case Right(NumberLiteralKey(key)) => assert(s"$value" === key)
          }
        }
        withClue(s"parsing: [ $value]") {
          Ftl.variant_key.parseAll(s"[ $value]") match {
              case Left(e) => fail(e.toString)
              case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
              case Right(NumberLiteralKey(key)) => assert(s"$value" === key)
          }
        }
        withClue(s"parsing: [ $value ]") {
          Ftl.variant_key.parseAll(s"[ $value ]") match {
              case Left(e) => fail(e.toString)
              case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
              case Right(NumberLiteralKey(key)) => assert(s"$value" === key)
          }
        }
        withClue(s"parsing: [$value ]") {
          Ftl.variant_key.parseAll(s"[$value ]") match {
              case Left(e) => fail(e.toString)
              case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
              case Right(NumberLiteralKey(key)) => assert(s"$value" === key)
          }
        }
      }
    }
    withClue("parsing signed/unsigned floating number") {
      var sign = "-" * r.nextInt(1)
      val a = r.nextInt(50)
      val b = r.nextInt(50)
      withClue(s"parsing: [$sign$a$b]") {
        Ftl.variant_key.parseAll(s"[$sign$a$b]") match {
            case Left(e) => fail(e.toString)
            case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
            case Right(NumberLiteralKey(key)) => assert(s"$sign$a$b" === key)
        }
      }
      withClue(s"parsing: [ $sign$a$b]") {
        Ftl.variant_key.parseAll(s"[ $sign$a$b]") match {
            case Left(e) => fail(e.toString)
            case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
            case Right(NumberLiteralKey(key)) => assert(s"$sign$a$b" === key)
        }
      }
      withClue(s"parsing: [ $sign$a$b ]") {
        Ftl.variant_key.parseAll(s"[ $sign$a$b ]") match {
            case Left(e) => fail(e.toString)
            case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
            case Right(NumberLiteralKey(key)) => assert(s"$sign$a$b" === key)
        }
      }
      withClue(s"parsing: [$sign$a$b ]") {
        Ftl.variant_key.parseAll(s"[$sign$a$b ]") match {
            case Left(e) => fail(e.toString)
            case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
            case Right(NumberLiteralKey(key)) => assert(s"$sign$a$b" === key)
        }
      }
    }
  }

  it should "parse identifier keys" in {
    for (i <- 'a' to 'z') {
      val random_tail = r.alphanumeric.take(r.nextInt(200)).mkString("");
      withClue(s"parsing: [ $i$random_tail]") {
        Ftl.variant_key.parseAll(s"[ $i$random_tail]") match {
          case Left(e) => fail(e.toString)
          case Right(IdentifierKey(id)) => assert(id.name === s"$i$random_tail")
          case Right(NumberLiteralKey(key)) => fail(s"number literal parsed: ${key}")
        }
      }
    }
    for (i <- 'A' to 'Z') {
      val random_tail = r.alphanumeric.take(r.nextInt(200)).mkString("");
      withClue(s"parsing: [$i$random_tail ]") {
        Ftl.variant_key.parseAll(s"[$i$random_tail ]") match {
          case Left(e) => fail(e.toString)
          case Right(IdentifierKey(id)) => assert(id.name === s"$i$random_tail")
          case Right(NumberLiteralKey(key)) => fail(s"number literal parsed: ${key}")
        }
      }
    }
  }

  "variant parser" should "parse numeral variants" in {
    for (variant <- List("\n[0] Zero", "\n[ 1 ] One", "\n[ 2] Two", "\n[3 ] Three", "\n[4]Four", "\n[5]    Five", "\n[6.0] Six", "\n[-7] Minus Seven", "\n[-7.0] Minus Seven Dot Zero")) {
      withClue(s"parsing: $variant") {
        Ftl.variant.parseAll(variant) match {
          case Left(e) => fail(e.toString)
          case Right(pars) => assert(pars.default == false && pars.key.isInstanceOf[NumberLiteralKey])
        }
      }
    }
  }

  it should "parse identifier variants" in {
    for (variant <- List("\n[zero] Zero", "\n[ more ] More", "\n[ once] Upon a Time", "\n[male ] Him", "\n[female]Her", "\n[null]    None")) {
      withClue(s"parsing: $variant") {
        Ftl.variant.parseAll(variant) match {
          case Left(e) => fail(e.toString)
          case Right(pars) => assert(pars.default == false && pars.key.isInstanceOf[IdentifierKey])
        }
      }
    }
  }

  "default_variant parser" should "parse numeral variants" in {
    for (variant <- List("\n*[0] Zero", "\n*[ 1 ] One", "\n*[ 2] Two", "\n*[3 ] Three", "\n*[4]Four", "\n*[5]    Five", "\n*[6.0] Six", "\n*[-7] Minus Seven", "\n*[-7.0] Minus Seven Dot Zero")) {
      withClue(s"parsing: $variant") {
        Ftl.default_variant.parseAll(variant) match {
          case Left(e) => fail(e.toString)
          case Right(pars) => assert(pars.default == true && pars.key.isInstanceOf[NumberLiteralKey])
        }
      }
    }
  }

  it should "parse identifier variants" in {
    for (variant <- List("\n*[zero] Zero", "\n*[ more ] More", "\n*[ once] Upon a Time", "\n*[male ] Him", "\n*[female]Her", "\n*[null]    None")) {
      withClue(s"parsing: $variant") {
        Ftl.default_variant.parseAll(variant) match {
          case Left(e) => fail(e.toString)
          case Right(pars) => assert(pars.default == true && pars.key.isInstanceOf[IdentifierKey])
        }
      }
    }
  }

  "variant_list parser" should "parse a list of variants (with one and only one default variant)" in {
    Ftl.variant_list.parseAll("\n[male] his stream\n[female] her stream\n*[other] their stream\n") match {
      case Left(e) => fail(e.toString)
      case Right(pars) => succeed
    }

    /* Should not parse more than a default variant! */
    Ftl.variant_list.parseAll("\n*[male] his stream\n[female] her stream\n*[other] their stream\n") match {
      case Left(e) => succeed
      case Right(pars) => fail(s"shoud not parse but parsed: $pars")
    }
  }

  "select_expression parser" should "parse a valid expression using variants (and a default one)" in {
    Ftl.select_expression.parseAll("$userGender ->\n[male] his stream\n[female] her stream\n*[other] their stream\n") match {
      case Left(e) => fail(e.toString)
      case Right(pars) => succeed
    }
    Ftl.select_expression.parseAll("$userGender ->\n*[male] his stream\n[female] her stream\n*[other] their stream\n") match {
      case Left(e) => succeed
      case Right(pars) => fail(s"shoud not parse but parsed: $pars")
    }
  }

  "named_argument parser" should "parse a given number/string literal as argument" in {
    for {
      named <- List("full_alpha", "one1digit", "a-function", "come0n ")
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1","-9.5555","-0.0009", " \"some space\"")
    } {
      withClue(s"arg name: $named, value: $value") {
        Ftl.named_argument.parseAll(s"$named:$value") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
    }
  }

  "argument parser" should "parse a given number/string literal as argument" in {
    for {
      named <- List("full_alpha", "one1digit", "a-function", "come0n ")
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1","-9.5555","-0.0009", " \"some space\"")
    } {
      withClue(s"arg name: $named, value: $value") {
        Ftl.argument.parseAll(s"$named:$value") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
    }
  }

  it should "parse a given inline expression (in theory; in practice only need number/string literals) as a positional argument" in {
    for {
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1","-9.5555","-0.0009")
    } {
      withClue(s"value: $value") {
        Ftl.argument.parseAll(s"$value") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
    }
  }

  "argument_list parser" should "parse 0 argument" in {
    Ftl.argument_list.parseAll("") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
    }
  }
  
  it should "parse 1 argument" in {
    for {
      named <- List("full_alpha", "one1digit", "a-function", "come0n ")
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1","-9.5555","-0.0009")
    } {
      withClue(s"named / arg name: $named, value: $value") {
        Ftl.argument_list.parseAll(s"$named:$value") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
      withClue(s"positional / value: $value") {
        Ftl.argument_list.parseAll(s"$value") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
    }
  }

  it should "parse more than 1 argument" in {
    for {
      named <- List("full_alpha", "one1digit", "a-function", "come0n ")
      named2 <- List(" full_alpha1", "one1digit2", " a-function3", "come0n4 ")
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1","-9.5555","-0.0009")
      value2 <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1","-9.5555","-0.0009")
    } {
      withClue(s"full-named / name: $named, value: $value, name2: $named, value2: $value2") {
        Ftl.argument_list.parseAll(s"$named:$value,$named2:$value2") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
      withClue(s"full-positional / value: $value, value2: $value2") {
        Ftl.argument_list.parseAll(s"$value,$value2") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
      withClue(s"mix3 / name: $named, value: $value, name2: $named, value2: $value2, value3: $value2") {
        Ftl.argument_list.parseAll(s"$named:$value,$named2:$value2,$value2") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
    }
  }

  "call_argument parser" should "parse 0 argument" in {
    Ftl.call_argument.parseAll("()") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
    }
    Ftl.call_argument.parseAll(" ()") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
    }
    Ftl.call_argument.parseAll(" ( )") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
    }
    Ftl.call_argument.parseAll(" (  )") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
    }
  }
  
  it should "parse 1 argument" in {
    for {
      named <- List(" full_alpha", "one1digit", "a-function", "come0n ")
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1","-9.5555","-0.0009")
    } {
      withClue(s"named / arg name: $named, value: $value") {
        Ftl.call_argument.parseAll(s" ($named:$value)") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
      withClue(s"positional / value: $value") {
        Ftl.call_argument.parseAll(s" ($value)") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
    }
  }

  it should "parse more than 1 argument" in {
    for {
      named <- List("full_alpha", "one1digit", "a-function", "come0n ")
      named2 <- List(" full_alpha1", "one1digit2", " a-function3", "come0n4 ")
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1","-9.5555","-0.0009")
      value2 <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1","-9.5555","-0.0009")
    } {
      withClue(s"full-named / name: $named, value: $value, name2: $named, value2: $value2") {
        Ftl.call_argument.parseAll(s"($named:$value,$named2:$value2)") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
      withClue(s"full-positional / value: $value, value2: $value2") {
        Ftl.call_argument.parseAll(s"($value,$value2)") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
      withClue(s"mix3 / name: $named, value: $value, name2: $named, value2: $value2, value3: $value2") {
        Ftl.call_argument.parseAll(s"($named:$value,$named2:$value2,$value2)") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
    }
  }

  "function_reference parser" should "parse a function call" in {
    withClue(s"no argument") {
      Ftl.function_reference.parseAll(s"alpha()") match {
        case Left(e) => fail(e.toString)
        case Right(pars) => succeed
      }
    }
    withClue(s"1 named") {
      Ftl.function_reference.parseAll(s"alpha(value:\"im an angel\")") match {
        case Left(e) => fail(e.toString)
        case Right(pars) => succeed
      }
    }
    withClue(s"1 positional") {
      Ftl.function_reference.parseAll(s"alpha(\"im an angel\")") match {
        case Left(e) => fail(e.toString)
        case Right(pars) => succeed
      }
    }
    withClue(s"1 positional + 1 named") {
      Ftl.function_reference.parseAll(s"alpha(\"im an angel\",default:\"true\")") match {
        case Left(e) => fail(e.toString)
        case Right(pars) => succeed
      }
    }
  }

  "message_reference" should "parse a reference to a message itself" in {
    Ftl.message_reference.parseAll(s"identifier_mes-sage") match {
        case Left(e) => fail(e.toString)
        case Right(MessageReference(id, attribute)) => assert(id.name === "identifier_mes-sage" && attribute.isEmpty)
    }
  }
  
  it should "parse a reference to a message attribute" in {
    Ftl.message_reference.parseAll(s"identifier_mes-sage.attri-bute_01") match {
        case Left(e) => fail(e.toString)
        case Right(MessageReference(id, Some(attribute))) => assert(id.name === "identifier_mes-sage" && attribute.name === "attri-bute_01")
        case Right(MessageReference(_, None)) => fail("parser missed the attribute identifier")
    }
  }

  "term_reference" should "parse a reference to a term itself" in {
    Ftl.term_reference.parseAll(s"-identifier_t-e-r-m") match {
        case Left(e) => fail(e.toString)
        case Right(TermReference(id, attribute, arguments)) => assert(id.name === "identifier_t-e-r-m" && attribute.isEmpty && arguments.isEmpty)
    }
  }
  
  it should "parse a reference to a term itself (using arguments)" in {
    Ftl.term_reference.parseAll(s"-identifier_t-e-r-m(10.2,gender:\"male\")") match {
        case Left(e) => fail(e.toString)
        case Right(TermReference(id, attribute, arguments)) => assert(id.name === "identifier_t-e-r-m" && attribute.isEmpty && arguments.isDefined)
    }
  }

  it should "parse a reference to a term attribute" in {
    Ftl.term_reference.parseAll(s"-identifier_t-e-r-m.attri-bute_01") match {
        case Left(e) => fail(e.toString)
        case Right(TermReference(id, Some(attribute), arguments)) => assert(id.name === "identifier_t-e-r-m" && attribute.name === "attri-bute_01" && arguments.isEmpty)
        case Right(TermReference(_,None, _)) => fail("parser missed attribute")
      }
  }
  
  it should "parse a reference to a term attribute (using arguments)" in {
    Ftl.term_reference.parseAll(s"-identifier_t-e-r-m.attri-bute_01(10.2,gender:\"male\")") match {
        case Left(e) => fail(e.toString)
        case Right(TermReference(id, Some(attribute), arguments)) => assert(id.name === "identifier_t-e-r-m" && attribute.name === "attri-bute_01" && arguments.isDefined)
        case Right(TermReference(_,None, _)) => fail("parser missed attribute")
        case Right(TermReference(_,_, None)) => fail("parser missed arguments")
    }
  }

  "inline_expression parser" should "parse a string literal" in {
    Ftl.inline_expression.parseAll("\"Hello dear\"") match {
        case Left(e) => fail(e.toString)
        case Right(StringLiteral(str)) => assert(str === "Hello dear")
        case Right(pars) => fail(s"not a string literal, parsed: $pars")
    }
  }

  it should "parse a number literal" in {
    Ftl.inline_expression.parseAll("-10.0") match {
        case Left(e) => fail(e.toString)
        case Right(NumberLiteral(str)) => assert(str === "-10.0")
        case Right(pars) => fail(s"not a number literal, parsed: $pars")
    }
  }

  it should "parse a function reference" in {
     Ftl.inline_expression.parseAll("function(num:-0001895.0, \"world\")") match {
        case Left(e) => fail(e.toString)
        case Right(FunctionReference(id, _)) => assert(id.name === "function")
        case Right(pars) => fail(s"not a function reference, parsed: $pars")
    }
  }

  it should "parse a message reference" in {
     Ftl.inline_expression.parseAll("message.attribute") match {
        case Left(e) => fail(e.toString)
        case Right(MessageReference(id, Some(attribute))) => assert(id.name === "message", attribute.name === "atribute")
        case Right(pars) => fail(s"not a message reference, parsed: $pars")
    }
  }

  it should "parse a term reference" in {
     Ftl.inline_expression.parseAll("-term.attribute") match {
        case Left(e) => fail(e.toString)
        case Right(TermReference(id, Some(attribute), None)) => assert(id.name === "term", attribute.name === "atribute")
        case Right(pars) => fail(s"not a term reference, parsed: $pars")
    }
  }

  it should "parse a variable reference" in {
     Ftl.inline_expression.parseAll("$variable") match {
        case Left(e) => fail(e.toString)
        case Right(VariableReference(id)) => assert(id.name === "variable")
        case Right(pars) => fail(s"not a variable reference, parsed: $pars")
    }
  }

  it should "parse a placeable expression" in {
     Ftl.inline_expression.parseAll("{\"hey\"}") match {
        case Left(e) => fail(e.toString)
        case Right(PlaceableExpr(_)) => succeed
        case Right(pars) => fail(s"not a placeable expression, parsed: $pars")
    }
  }

  "inline_placeable parser" should "parse a select expression" in {
     Ftl.inline_placeable.parseAll("{$userGender ->\n[male] his stream\n[female] her stream\n*[other] their stream\n}") match {
        case Left(e) => fail(e.toString)
        case Right(PlaceableExpr(_: Select)) => succeed
        case Right(pars) => fail(s"not a select expression, parsed: $pars")
    }
  }

  it should "parse an inline expression" in {
     Ftl.inline_placeable.parseAll("{$variable}") match {
        case Left(e) => fail(e.toString)
        case Right(PlaceableExpr(_: Inline)) => succeed
        case Right(pars) => fail(s"not an inline expression, parsed: $pars")
    }
  }

  "block_placeable parser" should "parse a select expression" in {
     Ftl.block_placeable.parseAll(" \n { $userGender ->\n[male] his stream\n[female] her stream\n*[other] their stream \n}") match {
        case Left(e) => fail(e.toString)
        case Right(PlaceableExpr(_: Select)) => succeed
        case Right(pars) => fail(s"not a select expression, parsed: $pars")
    }
  }

  it should "parse an inline expression" in {
     Ftl.block_placeable.parseAll(" \n {$variable}") match {
        case Left(e) => fail(e.toString)
        case Right(PlaceableExpr(_: Inline)) => succeed
        case Right(pars) => fail(s"not an inline expression, parsed: $pars")
    }
  }

  "pattern_element parser" should "parse an inline text" in {
     Ftl.pattern_element.parseAll("just some string") match {
        case Left(e) => fail(e.toString)
        case Right(TextElement(value)) => assert(value === "just some string")
        case Right(pars) => fail(s"not an inline text pattern, parsed: $pars")
    }
  }

  it should "parse a block text" in {
     Ftl.pattern_element.parseAll(" \n    as long as each new line is indented") match {
        case Left(e) => fail(e.toString)
        case Right(BlockTextElement(ident, Some(value))) => assert(ident === 4 && value === "as long as each new line is indented")
        case Right(pars) => fail(s"not a block text pattern\nparsed: $pars")
    }
  }

  it should "parse a block placeable" in {
     Ftl.pattern_element.parseAll(" \n {$variable}") match {
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

  "attribute parser" should "parse a valid attribute declaration" in {
    Ftl.attribute.parseAll("\n.attr1 = Attribute One") match {
        case Left(e) => fail(e.toString)
        case Right(attr) => assert(attr.id.name === "attr1")
    }
    Ftl.attribute.parseAll("\n.native = Project-Fluent ({$variable})") match {
        case Left(e) => fail(e.toString)
        case Right(attr) => assert(attr.id.name === "native")
    }
  }
 
  "message parser" should "parse a valid message declaration (without attribute)" in {
    Ftl.message.parseAll("welcome-message = Bienvenue à bord !") match {
        case Left(e) => fail(e.toString)
        case Right(msg) => assert(msg.id.name === "welcome-message" && msg.attributes.isEmpty && msg.value.isDefined && msg.comments.isEmpty)
    }
    Ftl.message.parseAll("thank-message = {-brand-name} vous remercie de votre support.") match {
        case Left(e) => fail(e.toString)
        case Right(msg) => assert(msg.id.name === "thank-message" && msg.attributes.isEmpty && msg.value.isDefined && msg.comments.isEmpty)
    }
  }

   it should "parse a valid message declaration (with only attributes)" in {
    Ftl.message.parseAll("status = \n.busy = Occupé\n.available = Disponible\n.offline = Déconnecté") match {
        case Left(e) => fail(e.toString)
        case Right(msg) => assert(msg.id.name === "status" && msg.attributes.length === 3 && msg.value.isEmpty && msg.comments.isEmpty)
    }
    Ftl.message.parseAll("status=\n.busy = Occupé\n.available = Disponible\n.offline = Déconnecté") match {
        case Left(e) => fail(e.toString)
        case Right(msg) => assert(msg.id.name === "status" && msg.attributes.length === 3 && msg.value.isEmpty && msg.comments.isEmpty)
    }
  }

  it should "parse a valid message declaration (with both a value & attributes)" in {
    Ftl.message.parseAll("status = Status de la connexion \n.busy = Occupé\n.available = Disponible\n.offline = Déconnecté") match {
        case Left(e) => fail(e.toString)
        case Right(msg) => assert(msg.id.name === "status" && msg.attributes.length === 3 && msg.value.isDefined && msg.comments.isEmpty)
    }
    Ftl.message.parseAll("status= Status de la connexion\n.busy = Occupé\n.available = Disponible\n.offline = Déconnecté") match {
        case Left(e) => fail(e.toString)
        case Right(msg) => assert(msg.id.name === "status" && msg.attributes.length === 3 && msg.value.isDefined && msg.comments.isEmpty)
    }
  }

  "term parser" should "parse a valid term declaration (without attribute)" in {
    Ftl.term.parseAll("-brand-name = Project Fluent for Scala") match {
        case Left(e) => fail(e.toString)
        case Right(term) => assert(term.id.name === "brand-name" && term.attributes.isEmpty && term.comments.isEmpty)
    }
    Ftl.term.parseAll("-https = https://{ $host }") match {
        case Left(e) => fail(e.toString)
        case Right(term) => assert(term.id.name === "https" && term.attributes.isEmpty && term.comments.isEmpty)
    }
  }

  it should "parse a valid term declaration (with attributes)" in {
    Ftl.term.parseAll("-brand-name = Project Fluent for Scala\n.nightly = Project Fluent (Nightly) for Scala\n.dev = Project Fluent (Developer Edition) for Scala") match {
        case Left(e) => fail(e.toString)
        case Right(term) => assert(term.id.name === "brand-name" && term.attributes.length === 2 && term.comments.isEmpty)
    }
    Ftl.term.parseAll("-brand-name = Aurora\n.gender = feminine") match {
        case Left(e) => fail(e.toString)
        case Right(term) => assert(term.id.name === "brand-name" && term.attributes.length === 1 && term.comments.isEmpty)
    }
  }

  "comment_line parser" should "parse resource comment" in {
    Ftl.comment_line.parseAll("### Res Comment") match {
      case Left(e) => fail(e.toString)
      case Right(ResourceComment(com)) => assert(com.content === "Res Comment")
      case Right(other) => fail(s"not a resource comment but parsed: $other")
    }
  }
  it should "parse group comment" in {
    Ftl.comment_line.parseAll("## Group Comment") match {
      case Left(e) => fail(e.toString)
      case Right(GroupComment(com)) => assert(com.content === "Group Comment")
      case Right(other) => fail(s"not a group comment but parsed: $other")
    }
  }
  it should "parse simple comment" in {
    Ftl.comment_line.parseAll("# Simple Comment") match {
      case Left(e) => fail(e.toString)
      case Right(Comment(com)) => assert(com.content === "Simple Comment")
      case Right(other) => fail(s"not a comment but parsed: $other")
    }
  }

  "entry parser" should "parse a message" in {
    Ftl.entry.parseAll("status = Status de la connexion\n.busy = Occupé\n.available = Disponible\n.offline = Déconnecté\n") match {
        case Left(e) => fail(e.toString)
        case Right(Message(msg)) => assert(msg.id.name === "status" && msg.attributes.length === 3 && msg.value.isDefined && msg.comments.isEmpty)
        case Right(other) => fail(s"not a message but parsed: $other")
    }
  }

  it should "parse a term" in {
    Ftl.entry.parseAll("-brand-name = Aurora\n.gender = feminine\r\n") match {
        case Left(e) => fail(e.toString)
        case Right(Term(term)) => assert(term.id.name === "brand-name" && term.attributes.length === 1 && term.comments.isEmpty)
        case Right(other) => fail(s"not a term but parsed: $other")
    }
  }

  it should "parse resource comment" in {
    Ftl.entry.parseAll("### Res Comment\n") match {
      case Left(e) => fail(e.toString)
      case Right(ResourceComment(com)) => assert(com.content === "Res Comment")
      case Right(other) => fail(s"not a resource comment but parsed: $other")
    }
  }

  it should "parse group comment" in {
    Ftl.entry.parseAll("## Group Comment\n") match {
      case Left(e) => fail(e.toString)
      case Right(GroupComment(com)) => assert(com.content === "Group Comment")
      case Right(other) => fail(s"not a group comment but parsed: $other")
    }
  }

  it should "parse simple comment" in {
    Ftl.entry.parseAll("# Simple Comment\n") match {
      case Left(e) => fail(e.toString)
      case Right(Comment(com)) => assert(com.content === "Simple Comment")
      case Right(other) => fail(s"not a simple comment but parsed: $other")
    }
  }

  "resource parser" should "parse a well-declared resource file" in {
    Ftl.resource.parseAll("# Simple Comment\n") match {
      case Left(e) => fail(e.toString)
      case Right(res) => println(res.show)
    }
    Ftl.resource.parseAll("### ENGLISH Resource (en_UK)\nwelcome = Bienvenue\n-brand-mark = Iron\nmotto = Hardened type constraints for Scala") match {
      case Left(e) => fail(e.toString)
      case Right(res) => println(res.show)
    }
    Ftl.resource.parseAll("## Closing tabs\n\ntabs-close-button = Close\ntabs-close-warning =\n    You are about to close {$tabCount} tabs.\n    Are you sure you want to continue?\n\n## Syncing\n\n-sync-brand-name = Firefox Account\n\nsync-dialog-title = {-sync-brand-name}\nsync-headline-title =\n    {-sync-brand-name}: The best way to bring\n    your data always with you\nsync-signedout-title =\n    Connect with your {-sync-brand-name}") match {
      case Left(e) => fail(e.toString)
      case Right(res) => println(res.show)
    }
  }
}

