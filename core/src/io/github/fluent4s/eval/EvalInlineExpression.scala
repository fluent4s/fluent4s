package io.github.fluent4s.eval

import cats.data.ValidatedNel
import cats.implicits._
import io.github.fluent4s.api.FluentValue.{Number, Text}
import io.github.fluent4s.api._
import io.github.fluent4s.ir._

trait EvalInlineExpression {

  implicit object InlineExpressionEvaluator extends Evaluator[RInlineExpression] {
    override def evaluate(input: RInlineExpression, key: String)(implicit context: EvalContext): Translation = input match {
      case RStringLiteral(value) => Text(value).validNel
      case RIntegerLiteral(value) => Number(value).validNel
      case RDecimalLiteral(value) => Number(value).validNel
      case RFunctionReference(resolved, args) =>
        (
          args.positional.map(evaluate(_, key)).sequence,
          args.named.map(tpl => evaluate(tpl._2, key).tupleLeft(tpl._1))
            .toList
            .sequence
            .map(_.toMap)
        )
          .mapN((_, _))
          .andThen(tpl => resolved.execute(context.locale, tpl._1, tpl._2))

      case RMessageReference(resolved) =>
        resolved
          .value
          .map(_.evaluate(key))
          .getOrElse(FluentValue.Empty.validNel)
      case RAttributeReference(resolved) => resolved.evaluate(key)
      case RTermReference(resolved, arguments) => ??? //TODO Term ref
      case RVariableReference(id) =>
        context.get(id)
          .toValidNel(TranslationError.MissingArgument(id))
      case RPlaceableExpr(expression) => expression.evaluate(key)
    }
  }
}
