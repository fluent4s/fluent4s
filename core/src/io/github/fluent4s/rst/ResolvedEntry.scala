package io.github.fluent4s.rst

import cats.implicits._
import io.github.fluent4s.api.ResolutionError
import io.github.fluent4s.ast._

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

  implicit object EntryResolver extends Resolver[ReferenceEntry, REntry] {

    override def resolve(input: ReferenceEntry)(implicit context: Context): Resolution[REntry] = input.body match {
      case m: FMessage => m.resolve
      case t: FTerm => t.resolve
    }
  }

  implicit object MessageResolver extends Resolver[FMessage, RMessage] {

    override def resolve(input: FMessage)(implicit context: Context): Resolution[RMessage] = (
      input.value.map(_.resolve).sequence,
      input.attributes.map(_.resolve).sequence.map(_.toMap)
      ).mapN(RMessage.apply)
  }

  implicit object TermResolver extends Resolver[FTerm, RTerm] {

    override def resolve(input: FTerm)(implicit context: Context): Resolution[RTerm] = (
      input.value.resolve,
      input.attributes.map(_.resolve).sequence.map(_.toMap)
      ).mapN(RTerm.apply)
  }
}
