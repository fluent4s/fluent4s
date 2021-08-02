import mill._, scalalib._, scalafmt._

object fluent_syntax extends SbtModule with ScalafmtModule {
  def scalaVersion = "3.0.0"
  def ivyDeps = Agg(
    ivy"org.typelevel::cats-parse::0.3.4".withDottyCompat(scalaVersion())
  )
}