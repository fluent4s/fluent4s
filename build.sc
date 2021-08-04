import mill._, scalalib._, scalafmt._, publish._

object fluent_syntax extends SbtModule with ScalafmtModule {
  def scalaVersion = "3.0.0"
  
  def ivyDeps = Agg(
    ivy"org.typelevel::cats-parse::0.3.4".withDottyCompat(scalaVersion()),
    ivy"org.typelevel::cats-core::2.3.0".withDottyCompat(scalaVersion())
  )

  def publishVersion = "0.0.1"

  def majorVersion: T[String] = "0"

  def artifactName = "fluent_syntax"

  def pomSettings = PomSettings(
    description = "fluent_syntax, part of fluent-scala, a collection of Scala modules implementing Project Fluent, is the host of the AST & Parser for FTL files.",
    organization = "io.github.fusetim",
    url = "https://github.com/fusetim/fluent-scala",
    licenses = Seq(License.`Apache-2.0`, License.MIT),
    versionControl = VersionControl.github("fusetim", "fluent-scala"),
    developers = Seq(
      Developer("fusetim", "Timothée B.", "https://github.com/fusetim")
    )
  )

  object test extends Tests with TestModule.ScalaTest with ScalafmtModule {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")
  }
}
