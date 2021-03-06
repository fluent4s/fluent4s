package io.github.fluent4s.test

import cats.implicits._
import io.github.fluent4s.api.FluentValue.Text
import io.github.fluent4s.api._
import FluentValue._
import com.ibm.icu.text.SimpleDateFormat
import io.github.fluent4s.parser.Ftl.Parser
import utest.{TestSuite, Tests, test}

import java.util.{Date, Locale}
import utest._

object IntegrationSuite extends TestSuite {

  val tests: Tests = Tests {

    val ftl =
      """# Simple things are simple.
        |hello-user = Hello, {$userName}!
        |
        |# Complex things are possible.
        |shared-photos =
        |    {$userName} {$photoCount ->
        |        [one] added a new photo
        |       *[other] added {$photoCount} new photos
        |    } to {$userGender ->
        |        [male] his stream
        |        [female] her stream
        |       *[other] their stream
        |    }.
        |
        |# Function test
        |today-is = Today is { DATETIME($date, weekday: "long", year: "numeric", month: "long", day: "numeric") }
        |""".stripMargin

    val resource = decode(ftl, Locale.ENGLISH)

    val args = Map(
      "userName" -> Text("Il_totore"),
      "photoCount" -> Number(1),
      "userGender" -> Text("male")
    )

    test("simple") {
      val simple = resource.andThen(_.getMessageWith("hello-user")(args))
      val simpleResult = "Hello, Il_totore!".validNel
      assert(simple == simpleResult)
    }

    test("complex") {
      val complex = resource.andThen(_.getMessageWith("shared-photos")(args))
      val complexResult = "Il_totore added a new photo to his stream.".validNel
      assert(complex == complexResult)
    }

    test("function") {
      val function = resource.andThen(_.getMessageWith("today-is")(args.updated("date", Number(0))))
      val format = new SimpleDateFormat("EEEE, MMMM dd, yyyy", Locale.ENGLISH)
      val functionResult = s"Today is ${format.format(new Date(0))}".validNel
      assert(function == functionResult)
    }
  }
}
