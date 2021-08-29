package io.github.fluent4s.ast

trait Entry {
  /**
   * A top-level node representing an entry of a [[FResource]].
   *
   * Every [[FEntry]] are a standalone element and the parser is able
   * to recover from errors by identifying the beggining of a new entry.
   * See [[Junk]] entries, which keep invalid FTL content.
   */
  sealed trait FEntry

  sealed trait EntryBody {

    val id: FIdentifier
  }

  sealed trait ReferenceEntry extends FEntry {

    val body: EntryBody

  }

  object ReferenceEntry {

    def unapply(value: ReferenceEntry): Option[EntryBody] = Some(value.body)
  }

  /**
   * [[Junk]] entry preserves invalid FTL content if FTL source contains
   * some.
   *
   * [[Junk]] contains the invalid FTL content in its body.
   * It can be safely discarded from a [[FResource]].
   *
   * @param content Invalid FTL content.
   */
  case class Junk(content: String) extends FEntry

  /**
   * [[Message]] entry is a [[FMessage]] entry.
   *
   * @param body An actual [[FMessage]].
   */
  case class Message(body: FMessage) extends ReferenceEntry

  /**
   * [[Term]] entry is a [[FTerm]] entry.
   *
   * @param body An actual [[FTerm]].
   */
  case class Term(body: FTerm) extends ReferenceEntry

  /**
   * [[Comment]] entry is a simple comment entry.
   *
   * A simple comment use `#` as its prefix. It is used to comment
   * only the following entry. There is a [[Comment]] post-processing,
   * the comment is added as a field of the next entry, if possible.
   *
   * Comment entries are defined in 3 forms, see [[GroupComment]]
   * and [[ResourceComment]].
   *
   * @param body An actual [[FComment]].
   */
  case class Comment(body: FComment) extends FEntry

  /**
   * [[GroupComment]] entry is a group comment entry.
   *
   * A group comment use `##` as its prefix. It is used to comment
   * a following group of entries. There is no post-processing, similar
   * to [[Comment]], associated with group comments.
   *
   * Comment entries are defined in 3 forms, see [[Comment]]
   * and [[ResourceComment]].
   *
   * @param body An actual [[FComment]].
   */
  case class GroupComment(body: FComment) extends FEntry

  /**
   * [[ResourceComment]] entry is a group comment entry.
   *
   * A resource comment use `###` as its prefix. It is used for general
   * information as the resource level. There is no post-processing, similar
   * to [[Comment]], associated with resource comments.
   *
   * Comment entries are defined in 3 forms, see [[GroupComment]]
   * and [[ResourceComment]].
   *
   * @param body An actual [[FComment]].
   */
  case class ResourceComment(body: FComment) extends FEntry

  /**
   * [[FMessage]] node is the most common [[FEntry]] in a FTL [[FResource]].
   *
   * A message is a localization unit with a [[FIdentifier]] unique within a given
   * [[FResource]], and a value or attributes with associated [[FPattern]].
   *
   * A message can contain a simple text value, or a compound combination of value
   * and attributes which together can be used to localize a complex User Interface
   * element.
   *
   * Finally, each [[FMessage]] may have an associated [[FComment]] if the previous
   * entry is a [[Comment]] entry.
   *
   * @param id         Unique [[FIdentifier]] within a [[FResource]].
   * @param value      Translation if it exists associated with this [[FMessage]].
   * @param attributes [[FAttribute]] associated translations for a given [[FMessage]].
   * @param comments   Comment entries preceding this [[FMessage]].
   */
  case class FMessage(
                         id: FIdentifier,
                         value: Option[FPattern],
                         attributes: List[FAttribute],
                         comments: Option[FComment]
                       ) extends EntryBody

  /**
   * A Fluent Term, similar to [[FMessage]].
   *
   * Similar to [[FMessage]], a [[FTerm]] represents a distinct concept in
   * Fluent. Therefore they are always constructed with a value.
   *
   * @param id         Unique [[FIdentifier]] within a [[FResource]].
   * @param value      Translation associated with this [[FTerm]].
   * @param attributes [[FAttribute]] associated translations for a given [[FTerm]].
   * @param comments   Comment entries preceding this [[FTerm]].
   */
  case class FTerm(
                      id: FIdentifier,
                      value: FPattern,
                      attributes: List[FAttribute],
                      comments: Option[FComment]
                    ) extends EntryBody

  /**
   * Fluent comment.
   *
   * In Fluent, comments may be associated to the [[FTerm]] or the [[FMessage]] that
   * follow them. This behavior exists only for Simple Comment ([[Comment]]).
   *
   * Fluent defined three Comment levels:
   *  - Simple comment, alone or associated to the following entry. (These ones use `#`.)
   *  - Group comment associated with a group of entries. (These ones use `##`.)
   *  - Resource comment associated with the whole resource. (These ones use `###`.)
   *
   * @param content Comment content
   */
  sealed class FComment(val content: String)
}