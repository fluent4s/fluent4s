package io.github.fluent4s.rst

trait Resolver[-A, B] {

  def resolve(input: A)(context: Context): Resolution[B]
}