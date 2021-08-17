package io.github.fluent4s.parser.ast

sealed abstract class FEntry

case class Junk(content: String) extends FEntry

case class Message(body: FMessage) extends FEntry

case class Term(body: FTerm) extends FEntry

case class Comment(body: FComment) extends FEntry

case class GroupComment(body: FComment) extends FEntry

case class ResourceComment(body: FComment) extends FEntry

sealed class FMessage(
    val id: FIdentifier,
    val value: Option[FPattern],
    val attributes: List[FAttribute],
    val comments: Option[FComment]
)

sealed class FTerm(
    val id: FIdentifier,
    val value: FPattern,
    val attributes: List[FAttribute],
    val comments: Option[FComment]
)

sealed class FComment(val content: String)
