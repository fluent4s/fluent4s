package io.github.fluent4s.parser.ftl

import cats.implicits.toShow
import io.github.fluent4s.ast._
import io.github.fluent4s.parser.UnitSpec
import io.github.fluent4s.parser.Ftl.{Parser => Ftl}
import org.scalatest.matchers.should._

import scala.collection.mutable

class FtlBlockExpressionSpec extends UnitSpec {
  private val r = util.Random

  /*
   * Select Expression related tests.
   */
  "select_expression parser" should "parse a valid expression using variants (and a default one)" in {
    Ftl.select_expression.parseAll(
      "$userGender ->\n[male] his stream\n[female] her stream\n*[other] their stream\n"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(pars) => succeed
    }
    Ftl.select_expression.parseAll(
      "$userGender -> \n[male] his stream\n[female] her stream\n*[other] their stream\n"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(pars) => succeed
    }
  }

  it should "not parse an expression using variants (with more than one default)" in {
    Ftl.select_expression.parseAll(
      "$userGender ->\n*[male] his stream\n[female] her stream\n*[other] their stream\n"
    ) match {
      case Left(e) => succeed
      case Right(pars) => fail(s"shoud not parse but parsed: $pars")
    }
  }

  it should "not parse an expression using variants (but malformated)" in {
    Ftl.select_expression.parseAll(
      "$userGender ->\n*[male] his stream\n[female] her stream\n*[other] their stream\n "
    ) match {
      case Left(e) => succeed
      case Right(pars) => fail(s"shoud not parse but parsed: $pars")
    }
  }

  "variant_list parser" should "parse a list of variants (with one and only one default variant)" in {
    Ftl.variant_list.parseAll(
      "\n[male] his stream\n[female] her stream\n*[other] their stream\n"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(pars) => succeed
    }

    Ftl.variant_list.parseAll(
      "\n  [male] his stream\n  [female] her stream\n  *[other] their stream\n"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(pars) => succeed
    }

    /* Should not parse more than a default variant! */
    Ftl.variant_list.parseAll(
      "\n*[male] his stream\n[female] her stream\n*[other] their stream\n"
    ) match {
      case Left(e) => succeed
      case Right(pars) => fail(s"shoud not parse but parsed: $pars")
    }
  }

  /*
   * FVariantKey related tests.
   */
  "variant_key parser" should "parse numeral keys" in {
    for (i <- 0 to 10) {
      withClue("parsing signed/unsigned integers") {
        val value = r.nextInt(200) - 100;
        withClue(s"parsing: [$value]") {
          Ftl.variant_key.parseAll(s"[$value]") match {
            case Left(e) => fail(e.toString)
            case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
            case Right(DecimalLiteralKey(id)) => fail(s"decimal passed parsed: $id")
            case Right(IntegerLiteralKey(key)) => assert(value === key)
          }
        }
        withClue(s"parsing: [ $value]") {
          Ftl.variant_key.parseAll(s"[ $value]") match {
            case Left(e) => fail(e.toString)
            case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
            case Right(DecimalLiteralKey(id)) => fail(s"decimal passed parsed: $id")
            case Right(IntegerLiteralKey(key)) => assert(value === key)
          }
        }
        withClue(s"parsing: [ $value ]") {
          Ftl.variant_key.parseAll(s"[ $value ]") match {
            case Left(e) => fail(e.toString)
            case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
            case Right(DecimalLiteralKey(id)) => fail(s"decimal passed parsed: $id")
            case Right(IntegerLiteralKey(key)) => assert(value === key)
          }
        }
        withClue(s"parsing: [$value ]") {
          Ftl.variant_key.parseAll(s"[$value ]") match {
            case Left(e) => fail(e.toString)
            case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
            case Right(DecimalLiteralKey(id)) => fail(s"decimal passed parsed: $id")
            case Right(IntegerLiteralKey(key)) => assert(value === key)
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
          case Right(DecimalLiteralKey(id)) => fail(s"decimal passed parsed: $id")
          case Right(IntegerLiteralKey(key)) => assert(s"$sign$a$b".toLong === key)
        }
      }
      withClue(s"parsing: [ $sign$a$b]") {
        Ftl.variant_key.parseAll(s"[ $sign$a$b]") match {
          case Left(e) => fail(e.toString)
          case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
          case Right(DecimalLiteralKey(id)) => fail(s"decimal passed parsed: $id")
          case Right(IntegerLiteralKey(key)) => assert(s"$sign$a$b".toLong === key)
        }
      }
      withClue(s"parsing: [ $sign$a$b ]") {
        Ftl.variant_key.parseAll(s"[ $sign$a$b ]") match {
          case Left(e) => fail(e.toString)
          case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
          case Right(DecimalLiteralKey(id)) => fail(s"decimal passed parsed: $id")
          case Right(IntegerLiteralKey(key)) => assert(s"$sign$a$b".toLong === key)
        }
      }
      withClue(s"parsing: [$sign$a$b ]") {
        Ftl.variant_key.parseAll(s"[$sign$a$b ]") match {
          case Left(e) => fail(e.toString)
          case Right(IdentifierKey(id)) => fail(s"identifier parsed: ${id.name}")
          case Right(DecimalLiteralKey(id)) => fail(s"decimal passed parsed: $id")
          case Right(IntegerLiteralKey(key)) => assert(s"$sign$a$b".toLong === key)
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
          case Right(IntegerLiteralKey(key)) => fail(s"integer literal parsed: ${key}")
          case Right(DecimalLiteralKey(key)) => fail(s"decimal literal parsed: ${key}")
        }
      }
    }
    for (i <- 'A' to 'Z') {
      val random_tail = r.alphanumeric.take(r.nextInt(200)).mkString("");
      withClue(s"parsing: [$i$random_tail ]") {
        Ftl.variant_key.parseAll(s"[$i$random_tail ]") match {
          case Left(e) => fail(e.toString)
          case Right(IdentifierKey(id)) => assert(id.name === s"$i$random_tail")
          case Right(IntegerLiteralKey(key)) => fail(s"integer literal parsed: ${key}")
          case Right(DecimalLiteralKey(key)) => fail(s"decimal literal parsed: ${key}")
        }
      }
    }
  }

  /*
   * FVariant (default or not) related tests.
   */
  "variant parser" should "parse numeral variants" in {
    for (
      variant <- List(
        "\n[0] Zero",
        "\n[ 1 ] One",
        "\n[ 2] Two",
        "\n[3 ] Three",
        "\n[4]Four",
        "\n[5]    Five"
      )
    ) {
      withClue(s"parsing: $variant") {
        Ftl.variant.parseAll(variant) match {
          case Left(e) => fail(e.toString)
          case Right(pars) =>
            assert(pars.default == false && pars.key.isInstanceOf[IntegerLiteralKey])
        }
      }
    }
  }

  it should "parse identifier variants" in {
    for (
      variant <- List(
        "\n[zero] Zero",
        "\n[ more ] More",
        "\n[ once] Upon a Time",
        "\n[male ] Him",
        "\n[female]Her",
        "\n[null]    None"
      )
    ) {
      withClue(s"parsing: $variant") {
        Ftl.variant.parseAll(variant) match {
          case Left(e) => fail(e.toString)
          case Right(pars) => assert(pars.default == false && pars.key.isInstanceOf[IdentifierKey])
        }
      }
    }
  }

  it should "not parse part of another variant" in {
    Ftl.variant.parseAll("\n  [female] her stream\n  *[other] their stream") match {
      case Left(e) => succeed
      case Right(pars) => fail("parsed but should not have: " + pars.show)
    }
  }

  "default_variant parser" should "parse numeral variants" in {
    for (
      variant <- List(
        "\n*[0] Zero",
        "\n*[ 1 ] One",
        "\n*[ 2] Two",
        "\n*[3 ] Three",
        "\n*[4]Four",
        "\n*[5]    Five",
        "\n*[-7] Minus Seven",
      )
    ) {
      withClue(s"parsing: $variant") {
        Ftl.default_variant.parseAll(variant) match {
          case Left(e) => fail(e.toString)
          case Right(pars) =>
            assert(pars.default == true && pars.key.isInstanceOf[IntegerLiteralKey])
        }
      }
    }
  }

  it should "parse identifier variants" in {
    for (
      variant <- List(
        "\n*[zero] Zero",
        "\n*[ more ] More",
        "\n*[ once] Upon a Time",
        "\n*[male ] Him",
        "\n*[female]Her",
        "\n*[null]    None+",
        "\n    *[indented] Indented"
      )
    ) {
      withClue(s"parsing: $variant") {
        Ftl.default_variant.parseAll(variant) match {
          case Left(e) => fail(e.toString)
          case Right(pars) => assert(pars.default == true && pars.key.isInstanceOf[IdentifierKey])
        }
      }
    }
  }
}
