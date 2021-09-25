package io.github.fluent4s.ir

trait Resolver[-A, B] {

  /**
   * Resolve the input.
   * @param input the AST node to resolve
   * @param context the resolution scope
   * @return the resolution result or a resolution errors
   */
  def resolve(input: A)(implicit context: ResolutionContext): Resolution[B]
}