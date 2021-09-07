package io.github.fluent4s.parser.ftl

import cats.implicits.toShow
import io.github.fluent4s.ast._
import io.github.fluent4s.parser.{Ftl, UnitSpec}
import org.scalatest.matchers.should._

import scala.collection.mutable

class FtlEntrySpec extends UnitSpec {
  /*
   * FMessage related tests.
   */
  "message parser" should "parse a valid message declaration (without attribute)" in {
    Ftl.message.parseAll("welcome-message = Bienvenue à bord !") match {
      case Left(e) => fail(e.toString)
      case Right(msg) => assert(
          msg.id.name === "welcome-message" && msg.attributes.isEmpty && msg.value.isDefined && msg.comments.isEmpty
        )
    }
    Ftl.message.parseAll("thank-message = {-brand-name} vous remercie de votre support.") match {
      case Left(e) => fail(e.toString)
      case Right(msg) => assert(
          msg.id.name === "thank-message" && msg.attributes.isEmpty && msg.value.isDefined && msg.comments.isEmpty
        )
    }
    Ftl.message.parseAll(
      "shared-photos =\n    { $username } {$photoCount ->\n           [one] added a new photo\n   *[other] added {$photoCount} new photos\n}"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(msg) => assert(
          msg.id.name === "shared-photos" && msg.attributes.isEmpty && msg.value.isDefined && msg.comments.isEmpty
        )
    }
  }

  it should "parse a valid message declaration (with only attributes)" in {
    Ftl.message.parseAll(
      "status = \n.busy = Occupé\n.available = Disponible\n.offline = Déconnecté"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(msg) => assert(
          msg.id.name === "status" && msg.attributes.length === 3 && msg.value.isEmpty && msg.comments.isEmpty
        )
    }
    Ftl.message.parseAll(
      "status=\n.busy = Occupé\n.available = Disponible\n.offline = Déconnecté"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(msg) => assert(
          msg.id.name === "status" && msg.attributes.length === 3 && msg.value.isEmpty && msg.comments.isEmpty
        )
    }
  }

  it should "parse a valid message declaration (with both a value & attributes)" in {
    Ftl.message.parseAll(
      "status = Status de la connexion \n.busy = Occupé\n.available = Disponible\n.offline = Déconnecté"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(msg) => assert(
          msg.id.name === "status" && msg.attributes.length === 3 && msg.value.isDefined && msg.comments.isEmpty
        )
    }
    Ftl.message.parseAll(
      "status= Status de la connexion\n.busy = Occupé\n.available = Disponible\n.offline = Déconnecté"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(msg) => assert(
          msg.id.name === "status" && msg.attributes.length === 3 && msg.value.isDefined && msg.comments.isEmpty
        )
    }
  }

  /*
   * FTerm related tests.
   */
  "term parser" should "parse a valid term declaration (without attribute)" in {
    Ftl.term.parseAll("-brand-name = Project Fluent for Scala") match {
      case Left(e) => fail(e.toString)
      case Right(term) =>
        assert(term.id.name === "brand-name" && term.attributes.isEmpty && term.comments.isEmpty)
    }
    Ftl.term.parseAll("-https = https://{ $host }") match {
      case Left(e) => fail(e.toString)
      case Right(term) =>
        assert(term.id.name === "https" && term.attributes.isEmpty && term.comments.isEmpty)
    }
  }

  it should "parse a valid term declaration (with attributes)" in {
    Ftl.term.parseAll(
      "-brand-name = Project Fluent for Scala\n.nightly = Project Fluent (Nightly) for Scala\n.dev = Project Fluent (Developer Edition) for Scala"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(term) => assert(
          term.id.name === "brand-name" && term.attributes.length === 2 && term.comments.isEmpty
        )
    }
    Ftl.term.parseAll("-brand-name = Aurora\n.gender = feminine") match {
      case Left(e) => fail(e.toString)
      case Right(term) => assert(
          term.id.name === "brand-name" && term.attributes.length === 1 && term.comments.isEmpty
        )
    }
  }

  /*
   * FComment (Comment, GroupComment and ResourceComment) related tests.
   */
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

  /*
   * FEntry related tests.
   */
  "entry parser" should "parse a message" in {
    Ftl.entry.parseAll(
      "status = Status de la connexion\n.busy = Occupé\n.available = Disponible\n.offline = Déconnecté\n"
    ) match {
      case Left(e) => fail(e.toString)
      case Right(Message(msg)) => assert(
          msg.id.name === "status" && msg.attributes.length === 3 && msg.value.isDefined && msg.comments.isEmpty
        )
      case Right(other) => fail(s"not a message but parsed: $other")
    }
  }

  it should "parse a term" in {
    Ftl.entry.parseAll("-brand-name = Aurora\n.gender = feminine\r\n") match {
      case Left(e) => fail(e.toString)
      case Right(Term(term)) => assert(
          term.id.name === "brand-name" && term.attributes.length === 1 && term.comments.isEmpty
        )
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
}
