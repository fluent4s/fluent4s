import mill._, scalalib._, scalafmt._, publish._

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

trait TestFramework extends TestModule {

  def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.10")

  def testFramework = "utest.runner.Framework"
}

object core extends Fluent4sModule {

  def moduleName = "core"

  def moduleVersion = "0"

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core::2.3.0"
  )

  def compileIvyDeps = Agg(
    ivy"com.ibm.icu:icu4j:69.1"
  )

  object test extends Tests with TestFramework {

    def ivyDeps = super.ivyDeps() ++ core.compileIvyDeps()

  }
}

object parser extends Fluent4sModule {

  def moduleName = "parser"

  def moduleVersion = "0"

  def moduleDeps = Seq(core)

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core::2.3.0",
    ivy"org.typelevel::cats-parse::0.3.4"
  )

  object test extends Tests with TestFramework { //TODO Migrate to UTest

    def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scalatest::scalatest:3.2.9")

    def testFramework = "org.scalatest.tools.Framework"
  }
}

object integrationTest extends ScalaModule with TestFramework {

  def scalaVersion = "2.13.6"

  def moduleDeps = Seq(core, parser)

  def ivyDeps = super.ivyDeps() ++ core.compileIvyDeps()
}
