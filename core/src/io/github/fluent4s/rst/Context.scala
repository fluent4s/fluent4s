package io.github.fluent4s.rst

case class Context(references: Map[String, REntry]) {

  def getReference(id: String): Option[REntry] = references.get(id)

  def withReference(id: String, value: REntry): Context = this.copy(this.references.updated(id, value))

  def toResource: RResource = RResource(references)
}

object Context {

  val Empty: Context = Context(Map.empty)
}
