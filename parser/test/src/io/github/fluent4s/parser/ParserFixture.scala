package io.github.fluent4s.parser

import io.circe.parser.decode
import io.circe.syntax._
import io.github.fluent4s.json.encoder._
import io.github.fluent4s.json.decoder._
import scala.io.Source
import java.io.FileNotFoundException
import cats.implicits._

import io.github.fluent4s.ast._
import scala.util.Try

import utest._

object ParserFixture extends TestSuite {

  def loadResource(path: String) : Try[Iterator[String]] = 
    Try(Source.fromResource(path).getLines)
  
  def loadFtl(filename: String): Try[String] = loadResource(s"fixtures/$filename.ftl").map(_.mkString("\n"))
  def loadJson(filename: String): Try[String] = loadResource(s"fixtures/$filename.json").map(_.mkString("\n"))

  val tests: Tests = Tests {
    test("comments") {
      test("simple_comment") {
        val left = Ftl.comment_line.parseAll(loadFtl("comment").get).right.get;
        val right = decode[FEntry](loadJson("comment").get).right.get;
        assert(left===right);
      }
    } 
  }

}