package io.github.fluent4s.ir

import com.ibm.icu.text.PluralRules
import io.github.fluent4s.api.FluentFunction

import java.util.Locale

/**
 * A context used by the resolution process.
 * @param locale the locale of the AST to resolve
 * @param references actual term/message references
 */
case class ResolutionContext(locale: Locale, references: Map[String, REntry], functions: Map[String, FluentFunction]) {

  lazy val pluralRules: PluralRules = PluralRules.forLocale(locale)

  def getReference(id: String): Option[REntry] = references.get(id)

  def withReference(id: String, value: REntry): ResolutionContext = this.copy(references = this.references.updated(id, value))

  def getFunction(id: String): Option[FluentFunction] = functions.get(id)

  def toResource: RResource = RResource(references)
}

object ResolutionContext {

  def fromValues(locale: Locale)(values: (String, REntry)*): ResolutionContext = ResolutionContext(locale, Map(values: _*), Map.empty)

  val Empty: ResolutionContext = ResolutionContext(Locale.ENGLISH, Map.empty, Map.empty)
}
