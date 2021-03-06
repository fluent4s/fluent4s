package io.github.fluent4s.test

import cats.data.Validated._
import cats.implicits._

import io.github.fluent4s.ir.ResolutionContext

import java.util.Locale
import io.github.fluent4s.ast._
import io.github.fluent4s.ir._

import utest._

object ResolutionSuite extends TestSuite {

  val tests: Tests = Tests {

    val context = ResolutionContext.fromValues(Locale.ENGLISH)(
      "msg" -> RMessage(None, Map.empty),
      "msgWithAttr" -> RMessage(None, Map(
        "attr" -> List.empty
      )),
      "term" -> RTerm(List.empty, Map.empty),
      "termWithAttr" -> RTerm(List.empty, Map(
        "attr" -> List.empty
      )),
    )

    test("messageReference") {
      assertMatch(MessageReference(FIdentifier("msg"), None).resolve(context)) { case Valid(RMessageReference(_)) => }
      assertMatch(MessageReference(FIdentifier("msgWithAttr"), None).resolve(context)) { case Valid(RMessageReference(_)) => }
      assert(MessageReference(FIdentifier("term"), None).resolve(context).isInvalid)
      assert(MessageReference(FIdentifier("???"), None).resolve(context).isInvalid)

      assertMatch(MessageReference(FIdentifier("msgWithAttr"), Some(FIdentifier("attr"))).resolve(context)) { case Valid(RAttributeReference(_)) => }
      assert(MessageReference(FIdentifier("msgWithAttr"), Some(FIdentifier("attr2"))).resolve(context).isInvalid)
      assert(MessageReference(FIdentifier("foo"), Some(FIdentifier("attr"))).resolve(context).isInvalid)
    }

    test("termReference") {
      assertMatch(TermReference(FIdentifier("term"), None, None).resolve(context)) { case Valid(RTermReference(_, _)) => }
      assertMatch(TermReference(FIdentifier("termWithAttr"), None, None).resolve(context)) { case Valid(RTermReference(_, _)) => }
      assert(TermReference(FIdentifier("msg"), None, None).resolve(context).isInvalid)
      assert(TermReference(FIdentifier("???"), None, None).resolve(context).isInvalid)

      assertMatch(TermReference(FIdentifier("termWithAttr"), Some(FIdentifier("attr")), None).resolve(context)) { case Valid(RAttributeReference(_)) => }
      assert(TermReference(FIdentifier("termWithAttr"), Some(FIdentifier("attr2")), None).resolve(context).isInvalid)
      assert(TermReference(FIdentifier("foo"), Some(FIdentifier("attr")), None).resolve(context).isInvalid)
    }

    test("variableReference") {
      assert(VariableReference(FIdentifier("foo")).resolve(ResolutionContext.Empty) == RVariableReference("foo").validNel)
    }
  }
}
