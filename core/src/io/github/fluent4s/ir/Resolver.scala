package io.github.fluent4s.ir

trait Resolver[-A, B] {

  def resolve(input: A)(implicit context: Context): Resolution[B]
}