package io.github.fluent4s.parser

import io.github.fluent4s.parser.UnitSpec;
import cats.implicits.toShow
import io.github.fluent4s.ast._
import org.scalatest.matchers.should._
import scala.collection.mutable

package object ftl {
  class FtlGlobalSpec extends UnitSpec {
    private val r = util.Random

    /*
     * FResource related tests.
     */
    "resource parser" should "parse a well-declared resource file" in {
      Ftl.resource.parseAll(
        "shared-photos =\n    { $username }           {$photoCount ->\n        [one] added a new photo\n       *[other] added {$photoCount} new photos\n    } to {$userGender ->\n        [male] his stream\n        [female] her stream\n       *[other] their stream\n    }."
      ) match {
        case Left(e) => fail(e.toString)
        case Right(res) => succeed;
      }
      Ftl.resource.parseAll("# Simple Comment\n") match {
        case Left(e) => fail(e.toString)
        case Right(res) => succeed; 
      }
      Ftl.resource.parseAll(
        "### ENGLISH Resource (en_UK)\nwelcome = Bienvenue\n-brand-mark = Iron\nmotto = Hardened type constraints for Scala"
      ) match {
        case Left(e) => fail(e.toString)
        case Right(res) => succeed; 
      }
      Ftl.resource.parseAll(
        "## Closing tabs\n\ntabs-close-button = Close\ntabs-close-warning =\n    You are about to close {$tabCount} tabs.\n    Are you sure you want to continue?\n\n## Syncing\n\n-sync-brand-name = Firefox Account\n\nsync-dialog-title = {-sync-brand-name}\nsync-headline-title =\n    {-sync-brand-name}: The best way to bring\n    your data always with you\nsync-signedout-title =\n    Connect with your {-sync-brand-name}"
      ) match {
        case Left(e) => fail(e.toString)
        case Right(res) => succeed;
      }
    }

    /*
     * FAttribute related tests.
     */
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

    /*
     * FIdentifier related tests.
     */
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
  }
}
