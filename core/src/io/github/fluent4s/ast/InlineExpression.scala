package io.github.fluent4s.ast

trait InlineExpression {

  /**
   * A subset of expressions which can be used as [[Placeable]],
   * [[FExpression]], or in [[FCallArguments]].
   */
  sealed trait FInlineExpression

  /**
   * A string literal.
   *
   * @param value The inner content as a String.
   */
  case class StringLiteral(value: String) extends FInlineExpression

  /**
   * An integer literal.
   *
   * @param value The inner value as a Long.
   */
  case class IntegerLiteral(value: Long) extends FInlineExpression


  /**
   * A decimal number literal.
   *
   * @param value The inner value as a Double.
   */
  case class DecimalLiteral(value: Double) extends FInlineExpression

  /**
   * A reference to a function.
   *
   * @param id        Unique [[FIdentifier]] within a [[FResource]].
   * @param arguments Optional [[FCallArguments]] to pass to the given function.
   */
  case class FunctionReference(id: FIdentifier, arguments: FCallArguments)
    extends FInlineExpression

  /**
   * A reference to a [[FMessage]].
   *
   * @param id        Unique [[FIdentifier]] within a [[FResource]].
   * @param attribute Optional [[FIdentifier]] for an [[FAttribute]] of
   *                  the given message.
   */
  case class MessageReference(
                               id: FIdentifier,
                               attribute: Option[FIdentifier]
                             ) extends FInlineExpression

  /**
   * A reference to a [[FTerm]].
   *
   * @param id        Unique [[FIdentifier]] within a [[FResource]].
   * @param attribute Optional [[FIdentifier]] for an [[FAttribute]] of
   *                  the given term.
   * @param arguments Optional [[FCallArguments]] to pass to the given term.
   */
  case class TermReference(
                            id: FIdentifier,
                            attribute: Option[FIdentifier],
                            arguments: Option[FCallArguments]
                          ) extends FInlineExpression

  /**
   * A reference to a variable.
   *
   * @param id Unique [[FIdentifier]] within a [[FResource]].
   */
  case class VariableReference(id: FIdentifier) extends FInlineExpression

  /**
   * A placeable which either contain another text literals or another expression.
   *
   * [[PlaceableExpr]] contains either a [[Select]] expression or an [[Inline]]
   * expression.
   *
   * @param expression The inner expression as [[FExpression]].
   */
  case class PlaceableExpr(expression: FExpression) extends FInlineExpression

  /**
   * List of [[FArgument]] for a [[FunctionReference]] or a [[TermReference]].
   *
   * Function and Term reference may contain a list of positional and named arguments
   * passed to them.
   *
   * @param positional List of positional arguments (as [[FInlineExpression]]).
   * @param named      List of named arguments (as [[NamedArgument]]).
   */
  case class FCallArguments(positional: List[FInlineExpression], named: List[NamedArgument])

  /**
   * Represents an argument to pass in a function as part of its [[FCallArguments]].
   *
   * See [[PositionalArgument]] and [[NamedArgument]].
   */
  sealed abstract class FArgument

  /**
   * A value as a positional argument used in [[FCallArguments]].
   *
   * @param value [[FInlineExpression]] as a value to pass
   */
  case class PositionalArgument(value: FInlineExpression) extends FArgument

  /**
   * A key-value pair argument used in [[FCallArguments]].
   *
   * @param name  Unique [[FIdentifier]] within a [[FResource]].
   * @param value [[FInlineExpression]] as a value to pass
   */
  case class NamedArgument(name: FIdentifier, value: FInlineExpression) extends FArgument

}