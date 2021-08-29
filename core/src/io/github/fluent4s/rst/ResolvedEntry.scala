package io.github.fluent4s.rst

import cats.implicits._
import io.github.fluent4s.ast._
import io.github.fluent4s.error.ResolutionError

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

    override def resolve(input: ReferenceEntry)(context: Context): Resolution[REntry] = input.body match {
      case m: FMessage => m.resolve(context)
      case t: FTerm => t.resolve(context)
    }
  }

  implicit object MessageResolver extends Resolver[FMessage, RMessage] {

    override def resolve(input: FMessage)(context: Context): Resolution[RMessage] = (
      input.value.map(_.resolve(context)).sequence,
      input.attributes.map(_.resolve(context)).sequence.map(_.toMap)
      ).mapN(RMessage.apply)
  }

  implicit object TermResolver extends Resolver[FTerm, RTerm] {

    override def resolve(input: FTerm)(context: Context): Resolution[RTerm] = (
      input.value.resolve(context),
      input.attributes.map(_.resolve(context)).sequence.map(_.toMap)
      ).mapN(RTerm.apply)
  }
}
