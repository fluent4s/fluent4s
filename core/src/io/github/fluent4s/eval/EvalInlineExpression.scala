package io.github.fluent4s.eval

import cats.data.ValidatedNel
import cats.implicits._
import io.github.fluent4s.api.FluentValue.{Number, Text}
import io.github.fluent4s.api._
import io.github.fluent4s.rst._

trait EvalInlineExpression {

  implicit object InlineExpressionEvaluator extends Evaluator[RInlineExpression] {
    override def evaluate(input: RInlineExpression, key: String)(implicit context: EvalContext): ValidatedNel[TranslationError, FluentValue] = input match {
      case RStringLiteral(value) => Text(value).validNel
      case RNumberLiteral(value) => Number(value).validNel
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
