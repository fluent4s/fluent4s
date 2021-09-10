package io.github.fluent4s.ir

import com.ibm.icu.text.PluralRules

import java.util.Locale

case class Context(locale: Locale, references: Map[String, REntry]) {

  lazy val pluralRules: PluralRules = PluralRules.forLocale(locale)

  def getReference(id: String): Option[REntry] = references.get(id)

  def withReference(id: String, value: REntry): Context = this.copy(references = this.references.updated(id, value))

  def toResource: RResource = RResource(references)
}

object Context {

  def fromValues(locale: Locale)(values: (String, REntry)*): Context = Context(locale, Map(values: _*))

  val Empty: Context = Context(Locale.ENGLISH, Map.empty)
}
