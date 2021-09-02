package io.github.fluent4s.rst

import com.ibm.icu.text.PluralRules

import java.util.Locale

case class Context(locale: Locale, references: Map[String, REntry]) {

  lazy val pluralRules: PluralRules = PluralRules.forLocale(locale)

  def getReference(id: String): Option[REntry] = references.get(id)

  def withReference(id: String, value: REntry): Context = this.copy(this.references.updated(id, value))

  def toResource: RResource = RResource(references)
}

object Context {

  def fromValues(values: (String, REntry)*): Context = Context(Map(values: _*))

  val Empty: Context = Context(Map.empty)
}
