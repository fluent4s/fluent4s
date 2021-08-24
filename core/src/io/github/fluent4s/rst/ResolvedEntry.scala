package io.github.fluent4s.rst

trait ResolvedEntry {

  sealed class REntry

  case class RMessage(
                       value: Option[RPattern],
                       attributes: Map[String, RPattern]
                     ) extends REntry

  case class RTerm(
                    value: RPattern,
                    attributes: Map[String, RPattern]
                  ) extends REntry
}
