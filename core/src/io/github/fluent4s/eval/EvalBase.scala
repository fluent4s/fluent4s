package io.github.fluent4s.eval

import cats.data.ValidatedNel
import cats.implicits._
import com.ibm.icu.text.PluralRules
import com.ibm.icu.text.PluralRules.FixedDecimal
import io.github.fluent4s.api.{FluentValue, TranslationError}
import FluentValue._
import io.github.fluent4s.ir._


trait EvalBase {


  //noinspection ScalaDeprecation
  @SuppressWarnings(Array("deprecation")) //ICU4J's internal API is required to check plural case
  implicit object ExpressionEvaluator extends Evaluator[RExpression] {
    override def evaluate(input: RExpression, key: String)(implicit context: EvalContext): ValidatedNel[TranslationError, FluentValue] = input match {

      case RSelect(selector, variants) => selector.evaluate(key).andThen { selectorInput =>
        val rules = PluralRules.forLocale(context.locale)
        variants.find { variant =>
          if (variant.default) true
          else (selectorInput, variant.key) match {
            case (_, RWordKey(value)) => value.equals(selectorInput.asString)
            case (Number(n), RPluralKey(value)) => rules.matches(new FixedDecimal(n), value)
            case (Number.Integer(n), RIntegerLiteralKey(value)) => value == n
            case (Number.Decimal(n), RDecimalLiteralKey(value)) => value == n
            case _ => false
          }
        }
          .toValidNel(new TranslationError(s"No suitable case found for value $selectorInput"))
          .andThen(_.value.evaluate(key))
      }

      case RInline(body) => body.evaluate(key)
    }
  }

  implicit object ResourceEvaluator extends Evaluator[RResource] {

    override def evaluate(input: RResource, key: String)(implicit context: EvalContext): ValidatedNel[TranslationError, FluentValue] =
      input
        .body
        .get(key)
        .toValidNel(new TranslationError(s"Unknown entry: $key"))
        .andThen(_.evaluate(key))
  }
}
