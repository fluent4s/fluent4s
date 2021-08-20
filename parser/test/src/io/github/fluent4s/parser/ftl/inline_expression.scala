package io.github.fluent4s.parser.ftl

import cats.implicits.toShow
import io.github.fluent4s.ast._
import io.github.fluent4s.parser.{Ftl, UnitSpec}
import org.scalatest.matchers.should._

import scala.collection.mutable

class FtlInlineExpressionSpec extends UnitSpec {
    /*
     * NamedArgument related tests.
     */
    "named_argument parser" should "parse a given number/string literal as argument" in {
    for {
      named <- List("full_alpha", "one1digit", "a-function", "come0n ")
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1", "-9.5555", "-0.0009", " \"some space\"")
    } {
      withClue(s"arg name: $named, value: $value") {
        Ftl.named_argument.parseAll(s"$named:$value") match {
          case Left(e) => fail(e.toString)
          case Right(pars) => succeed
        }
      }
    }
  }

  /*
   * FArgument related tests.
   */
  "argument parser" should "parse a given number/string literal as argument" in {
    for {
      named <- List("full_alpha", "one1digit", "a-function", "come0n ")
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1", "-9.5555", "-0.0009", " \"some space\"")
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
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1", "-9.5555", "-0.0009")
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
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1", "-9.5555", "-0.0009")
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
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1", "-9.5555", "-0.0009")
      value2 <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1", "-9.5555", "-0.0009")
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

  /*
   * FCallArgument related tests.
   */
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
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1", "-9.5555", "-0.0009")
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
      value <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1", "-9.5555", "-0.0009")
      value2 <- List("\"a string literal\"", "\"Unicode2 \\U01F910\"", "\"Unicode3 \\u0805\"", "\"escaped slash \\\\\"", "\"escaped quote \\\"\"", "0.0", "-0.0", "1", "-1", "-9.5555", "-0.0009")
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

  /*
   * FunctionReference related tests.
   */
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
  /*
  * MessageReference related tests.
  */
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

  /*
   * TermReference related tests.
   */
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
      case Right(TermReference(_, None, _)) => fail("parser missed attribute")
    }
  }

  it should "parse a reference to a term attribute (using arguments)" in {
    Ftl.term_reference.parseAll(s"-identifier_t-e-r-m.attri-bute_01(10.2,gender:\"male\")") match {
      case Left(e) => fail(e.toString)
      case Right(TermReference(id, Some(attribute), arguments)) => assert(id.name === "identifier_t-e-r-m" && attribute.name === "attri-bute_01" && arguments.isDefined)
      case Right(TermReference(_, None, _)) => fail("parser missed attribute")
      case Right(TermReference(_, _, None)) => fail("parser missed arguments")
    }
  }

  /*
   * FInlineExpression related tests.
   */
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
}