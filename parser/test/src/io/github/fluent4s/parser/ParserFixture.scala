package io.github.fluent4s.parser

import io.circe.parser.decode
import io.circe.syntax._
import io.github.fluent4s.json.encoder._
import io.github.fluent4s.json.decoder._
import scala.io.Source
import java.io.FileNotFoundException
import java.io.{File, FileWriter, BufferedWriter}
import cats.implicits._

import io.github.fluent4s.ast._
import scala.util.Try

import utest._

object ParserFixture extends TestSuite {


  def loadResource(path: String) : Try[Iterator[String]] = 
    Try(Source.fromResource(path).getLines)
  
  def loadFtl(filename: String): Try[String] = loadResource(s"fixtures/$filename.ftl").map(_.mkString("\n"))
  def loadJson(filename: String): Try[String] = loadResource(s"fixtures/$filename.json").map(_.mkString("\n"))

  val tests = Tests {
    'parsing - {
      def check(name: String) = {
        val ftl =  Ftl.resource.parseAll(loadFtl(name).get);
        println(ftl);
        val left = ftl.right.get;
      
        val file = new File(s"/mnt/veracrypt1/Documents/Dev/fluent4s/parser/test/resources/fixtures/$name.json")
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(left.asJson.toString)
        bw.close()
        /*val right = decode[FResource](loadJson(name).get).right.get;
        assert(left===right);*/
      }

      'any_char - check("any_char")
      'astral - check("astral")
      'call_expressions - check("call_expressions")
      'callee_expressions - check("callee_expressions")
      'comments - check("comments")
      'cr - check("cr")
      'crlf - check("crlf")
      'eof_comment - check("eof_comment")
      'eof_empty - check("eof_empty")
      'eof_id_equals - check("eof_id_equals")
      'eof_id - check("eof_id")
      'eof_junk - check("eof_junk")
      'eof_value - check("eof_value")
      'escaped_characters - check("escaped_characters")
      'junk - check("junk")
      'leading_dots - check("leading_dots")
      'literal_expressions - check("literal_expressions")
      'member_expressions - check("member_expressions")
      'messages - check("messages")
      'mixed_entries - check("mixed_entries")
      'multiline_values - check("multiline_values")
      'numbers - check("numbers")
      'obsolete - check("obsolete")
      'placeables - check("placeables")
      'reference_expressions - check("reference_expressions")
      'select_expressions - check("select_expressions")
      'select_indent - check("select_indent")
      'sparse_entries - check("sparse_entries")
      'sparse_entries - check("special_chars")
      'tab - check("tab")
      'term_parameters - check("term_parameters")
      'terms - check("terms")
      'variables - check("variables")
      'variant_keys - check("variant_keys")
      'whitespace_in_value - check("whitespace_in_value")
      'zero_length - check("zero_length")
    }
  }

}