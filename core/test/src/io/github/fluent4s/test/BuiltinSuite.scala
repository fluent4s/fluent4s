package io.github.fluent4s.test

import cats.implicits._
import com.ibm.icu.text.SimpleDateFormat
import io.github.fluent4s.api.FluentValue
import io.github.fluent4s.api.function.DateTimeFunction
import io.github.fluent4s.api.FluentValue._
import utest._

import java.util.{Date, Locale}

object BuiltinSuite extends TestSuite {

  val tests: Tests = Tests {
    test("datetime"){

      def check(locale: Locale, time: Long = 0, style: String = "", named: Map[String, FluentValue] = Map.empty)(output: String): Unit = {
        val safeStyle = if(style.isEmpty) List.empty else List(Text(style))
        val formatted = DateTimeFunction.execute(locale, List(Number(time)) ++ safeStyle, named)
        val outputFormat = new SimpleDateFormat(output, locale)
        val expected = Text(outputFormat.format(new Date(time))).validNel
        assert(formatted == expected)
      }

      test("dateStyle") {
        test - check(Locale.US, style = "short")("M/d/yy")
        test - check(Locale.US, style = "medium")("MMM d, yyyy")
        test - check(Locale.US, style = "long")("MMMM d, yyyy")
        test - check(Locale.US, style = "full")("EEEE, MMMM d, yyyy")
      }

      test("fieldLength") {
        test - check(Locale.GERMAN, named = Map(
          "weekday" -> Text("long"),
          "year" -> Text("numeric"),
          "month" -> Text("long"),
          "day" -> Text("numeric")
        ))("EEEE, dd. MMMM yyyy")

        test - check(Locale.US, named = Map(
          "weekday" -> Text("long"),
          "year" -> Text("numeric"),
          "month" -> Text("long"),
          "day" -> Text("numeric"),
          "timeZoneName" -> Text("short")
        ))("EEEE, MMMM dd, yyyy, zzz")

        test - check(Locale.forLanguageTag("en-AU"), named = Map(
          "hour12" -> Text("numeric"),
          "minute" -> Text("numeric"),
          "second" -> Text("numeric"),
          "timeZoneName" -> Text("short")
        ))("h:mm:ss aa zzz")
      }
    }
  }
}
