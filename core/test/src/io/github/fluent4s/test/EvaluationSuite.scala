package io.github.fluent4s.test

import cats.implicits._
import io.github.fluent4s.api.FluentValue.{Number, Text}
import io.github.fluent4s.ast._
import io.github.fluent4s.eval._
import io.github.fluent4s.ir._
import utest.{TestSuite, Tests, assert, test}

import java.util.Locale

object EvaluationSuite extends TestSuite {

  val tests: Tests = Tests {

    test("unit") {
      val defaultKey: String = ""
      implicit val defaultContext: EvalContext = EvalContext(Locale.ENGLISH, Map(
        "attr" -> Text("foo")
      ))

      test("leaf") {
        test("inlineExpression") {
          assert(RStringLiteral("test").evaluate(defaultKey) == Text("test").validNel)
          assert(RIntegerLiteral(1).evaluate(defaultKey) == Number(1).validNel)
          assert(RDecimalLiteral(1d).evaluate(defaultKey) == Number(1d).validNel)
          assert(RVariableReference("attr").evaluate(defaultKey) == Text("foo").validNel)
        }

        test("entry") {
          assert(RTextElement("test").evaluate(defaultKey) == Text("test").validNel)
        }
      }
    }

    test("integration") {

      //IR of Fluent's first example: https://projectfluent.org/
      val resolved = RResource(Map(
        "hello-user" -> RMessage(
          Some(List(
            RTextElement("Hello, "),
            RPlaceable(RInline(RVariableReference("userName"))),
            RTextElement("!")
          )),
          Map.empty
        ),
        "shared-photos" -> RMessage(
          Some(List(
            RPlaceable(RInline(RVariableReference("userName"))),
            RTextElement(" "),
            RPlaceable(RSelect(
              RVariableReference("photoCount"),
              List(
                RVariant(
                  RPluralKey("one"),
                  List(RTextElement("added a new photo")),
                  default = false
                ),
                RVariant(
                  RPluralKey("other"),
                  List(
                    RTextElement("added "),
                    RPlaceable(RInline(RVariableReference("photoCount"))),
                    RTextElement(" new photos")
                  ),
                  default = true
                )
              )
            )),
            RTextElement(" to "),
            RPlaceable(RSelect(
              RVariableReference("userGender"),
              List(
                RVariant(
                  RWordKey("male"),
                  List(RTextElement("his stream")),
                  default = false
                ),
                RVariant(
                  RWordKey("female"),
                  List(RTextElement("her stream")),
                  default = false
                ),
                RVariant(
                  RWordKey("other"),
                  List(RTextElement("their stream")),
                  default = true
                )
              )
            )),
            RTextElement(".")
          )),
          Map.empty
        )
      ))

      test("withVariable") {

        implicit val context: EvalContext = EvalContext(Locale.ENGLISH, Map(
          "userName" -> Text("Il_totore")
        ))

        val result = resolved.evaluate("hello-user")

        assert(result == Text("Hello, Il_totore!").validNel)
      }

      test("withSelector") {

        def evaluate(count: Int, gender: String): Unit = {

          val countPart = if(count == 1) "added a new photo" else s"added $count new photos"
          val genderPart = gender match {

            case "male" => "his stream"
            case "female" => "her stream"
            case _ => "their stream"
          }

          implicit val context: EvalContext = EvalContext(Locale.ENGLISH, Map(
            "userName" -> Text("Il_totore"),
            "photoCount" -> Number(count),
            "userGender" -> Text(gender)
          ))

          val result = resolved.evaluate("shared-photos")
          val expected = Text(s"Il_totore $countPart to $genderPart.").validNel

          assert(result == expected)
        }

        for {
          count <- 0 to 2
          gender <- Seq("male", "female", "non-binary")
        } evaluate(count, gender)
      }
    }
  }
}
