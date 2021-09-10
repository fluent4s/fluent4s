package io.github.fluent4s.test

import cats.implicits._
import io.github.fluent4s.api.FluentValue.{Number, Text}
import io.github.fluent4s.eval._
import io.github.fluent4s.ir._
import utest.{TestSuite, Tests, assert, test}

import java.util.Locale

object EvaluationSuite extends TestSuite {

  val tests: Tests = Tests {

    val defaultKey: String = ""
    implicit val defaultContext: EvalContext = EvalContext(Locale.ENGLISH, Map(
      "attr" -> Text("foo")
    ))

    test("leaf") {
      test("inlineExpression") {
        assert(RStringLiteral("test").evaluate(defaultKey) == Text("test").validNel)
        assert(RNumberLiteral(1).evaluate(defaultKey) == Number(1).validNel)
        assert(RVariableReference("attr").evaluate(defaultKey) == Text("foo").validNel)
      }

      test("entry") {
        assert(RTextElement("test").evaluate(defaultKey) == Text("test").validNel)
      }
    }
  }
}
