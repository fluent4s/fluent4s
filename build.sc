import mill._
import parser.ivy
import scalalib._
import scalafmt._
import publish._

val projectName = "fluent4s"
val majorVersion = "0.1"

trait Fluent4sModule extends ScalaModule with ScalafmtModule with PublishModule {

  def moduleName: String

  def moduleVersion: String

  def scalaVersion = "2.13.6"

  def artifactName = s"$majorVersion-$moduleName"

  def publishVersion = s"$majorVersion.$moduleVersion"

  def pomSettings = PomSettings(
    description = "A collection of Scala modules implementing Project Fluent, is the host of the AST & Parser for FTL files.",
    organization = "io.github.fluent4s",
    url = "https://github.com/fluent4s/fluent4s",
    licenses = Seq(License.`Apache-2.0`, License.MIT),
    versionControl = VersionControl.github("fluent4s", "fluent4s"),
    developers = Seq(
      Developer("fusetim", "Timothée B.", "https://github.com/fusetim"),
      Developer("Iltotore", "Raphaël FROMENTIN", "https://github.com/Iltotore")
    )
  )
}

trait ScalaTest extends TestModule with ScalafmtModule {

  def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")

  def testFramework = "org.scalatest.tools.Framework"
}

object parser extends Fluent4sModule {

  def moduleName = "parser"

  def moduleVersion = "0"

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-parse::0.3.4",
    ivy"org.typelevel::cats-core::2.3.0"
  )

  object test extends Tests with ScalaTest
}
