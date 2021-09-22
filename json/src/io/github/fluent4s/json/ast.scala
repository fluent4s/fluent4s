package io.github.fluent4s.json

import io.circe._, io.circe.generic.semiauto._
import io.github.fluent4s.ast._

object decoder {

/* Decoders for AST Bases */
implicit val resourceDecoder: Decoder[FResource] = deriveDecoder[FResource]
implicit val attributeDecoder: Decoder[FAttribute] = deriveDecoder[FAttribute] 
implicit val identifierDecoder: Decoder[FIdentifier] = deriveDecoder[FIdentifier]
implicit val expressionDecoder: Decoder[FExpression] = deriveDecoder[FExpression]
implicit val selectDecoder: Decoder[Select] = deriveDecoder[Select] 
implicit val inlineDecoder: Decoder[Inline] = deriveDecoder[Inline] 

/* Decoders for Block Expressions */
implicit val variantDecoder: Decoder[FVariant] = deriveDecoder[FVariant] 
implicit val variantKeyDecoder: Decoder[FVariantKey] = deriveDecoder[FVariantKey]
implicit val identifierKeyDecoder: Decoder[IdentifierKey] = deriveDecoder[IdentifierKey]
implicit val numberLiteralKeyDecoder: Decoder[NumberLiteralKey] = deriveDecoder[NumberLiteralKey]

/* Decoders for Entries */
implicit val entryDecoder: Decoder[FEntry] = deriveDecoder[FEntry]
implicit val messageDecoder: Decoder[FMessage] = deriveDecoder[FMessage] 
implicit val termDecoder: Decoder[FTerm] = deriveDecoder[FTerm] 
implicit val commentDecoder: Decoder[FComment] = deriveDecoder[FComment]
implicit val junkDecoder: Decoder[Junk] = deriveDecoder[Junk] 
implicit val scommentDecoder: Decoder[Comment] = deriveDecoder[Comment] 
implicit val gcommentDecoder: Decoder[GroupComment] = deriveDecoder[GroupComment] 
implicit val rcommentDecoder: Decoder[ResourceComment] = deriveDecoder[ResourceComment] 

/* Decoders for Inline Expressions */
implicit val inlineExprDecoder: Decoder[FInlineExpression] = deriveDecoder[FInlineExpression] 
implicit val stringLiteralExprDecoder: Decoder[StringLiteral] = deriveDecoder[StringLiteral] 
implicit val numberLiteralExprDecoder: Decoder[NumberLiteral] = deriveDecoder[NumberLiteral] 
implicit val functionRefExprDecoder: Decoder[FunctionReference] = deriveDecoder[FunctionReference] 
implicit val messageRefExprDecoder: Decoder[MessageReference] = deriveDecoder[MessageReference] 
implicit val termRefExprDecoder: Decoder[TermReference] = deriveDecoder[TermReference] 
implicit val variableRefExprDecoder: Decoder[VariableReference] = deriveDecoder[VariableReference] 
implicit val placeableExprExprDecoder: Decoder[PlaceableExpr] = deriveDecoder[PlaceableExpr] 
implicit val callArgExprDecoder: Decoder[FCallArguments] = deriveDecoder[FCallArguments] 
implicit val argumentExprDecoder: Decoder[FArgument] = deriveDecoder[FArgument] 
implicit val positionalArgExprDecoder: Decoder[PositionalArgument] = deriveDecoder[PositionalArgument] 
implicit val namedArgExprDecoder: Decoder[NamedArgument] = deriveDecoder[NamedArgument] 

/* Decoders for Patterns */
implicit val patternDecoder: Decoder[FPattern] = deriveDecoder[FPattern] 
implicit val patternElementDecoder: Decoder[FPatternElement] = deriveDecoder[FPatternElement] 
implicit val textElementDecoder: Decoder[TextElement] = deriveDecoder[TextElement] 
implicit val blockTextElementDecoder: Decoder[BlockTextElement] = deriveDecoder[BlockTextElement] 
implicit val placeableDecoder: Decoder[Placeable] = deriveDecoder[Placeable] 
}
object encoder {

/* Encoders for AST Bases */
implicit val resourceEncoder: Encoder[FResource] = deriveEncoder[FResource]
implicit val attributeEncoder: Encoder[FAttribute] = deriveEncoder[FAttribute] 
implicit val identifierEncoder: Encoder[FIdentifier] = deriveEncoder[FIdentifier]
implicit val expressionEncoder: Encoder[FExpression] = deriveEncoder[FExpression]
implicit val selectEncoder: Encoder[Select] = deriveEncoder[Select] 
implicit val inlineEncoder: Encoder[Inline] = deriveEncoder[Inline] 

/* Encoders for Block Expressions */
implicit val variantEncoder: Encoder[FVariant] = deriveEncoder[FVariant] 
implicit val variantKeyEncoder: Encoder[FVariantKey] = deriveEncoder[FVariantKey]
implicit val identifierKeyEncoder: Encoder[IdentifierKey] = deriveEncoder[IdentifierKey]
implicit val numberLiteralKeyEncoder: Encoder[NumberLiteralKey] = deriveEncoder[NumberLiteralKey]

/* Encoders for Entries */
implicit val entryEncoder: Encoder[FEntry] = deriveEncoder[FEntry]
implicit val messageEncoder: Encoder[FMessage] = deriveEncoder[FMessage] 
implicit val termEncoder: Encoder[FTerm] = deriveEncoder[FTerm] 
implicit val commentEncoder: Encoder[FComment] = deriveEncoder[FComment]
implicit val junkEncoder: Encoder[Junk] = deriveEncoder[Junk] 
implicit val scommentEncoder: Encoder[Comment] = deriveEncoder[Comment] 
implicit val gcommentEncoder: Encoder[GroupComment] = deriveEncoder[GroupComment] 
implicit val rcommentEncoder: Encoder[ResourceComment] = deriveEncoder[ResourceComment] 

/* Encoders for Inline Expressions */
implicit val inlineExprEncoder: Encoder[FInlineExpression] = deriveEncoder[FInlineExpression] 
implicit val stringLiteralExprEncoder: Encoder[StringLiteral] = deriveEncoder[StringLiteral] 
implicit val numberLiteralExprEncoder: Encoder[NumberLiteral] = deriveEncoder[NumberLiteral] 
implicit val functionRefExprEncoder: Encoder[FunctionReference] = deriveEncoder[FunctionReference] 
implicit val messageRefExprEncoder: Encoder[MessageReference] = deriveEncoder[MessageReference] 
implicit val termRefExprEncoder: Encoder[TermReference] = deriveEncoder[TermReference] 
implicit val variableRefExprEncoder: Encoder[VariableReference] = deriveEncoder[VariableReference] 
implicit val placeableExprExprEncoder: Encoder[PlaceableExpr] = deriveEncoder[PlaceableExpr] 
implicit val callArgExprEncoder: Encoder[FCallArguments] = deriveEncoder[FCallArguments] 
implicit val argumentExprEncoder: Encoder[FArgument] = deriveEncoder[FArgument] 
implicit val positionalArgExprEncoder: Encoder[PositionalArgument] = deriveEncoder[PositionalArgument] 
implicit val namedArgExprEncoder: Encoder[NamedArgument] = deriveEncoder[NamedArgument] 

/* Encoders for Patterns */
implicit val patternEncoder: Encoder[FPattern] = deriveEncoder[FPattern] 
implicit val patternElementEncoder: Encoder[FPatternElement] = deriveEncoder[FPatternElement] 
implicit val textElementEncoder: Encoder[TextElement] = deriveEncoder[TextElement] 
implicit val blockTextElementEncoder: Encoder[BlockTextElement] = deriveEncoder[BlockTextElement] 
implicit val placeableEncoder: Encoder[Placeable] = deriveEncoder[Placeable] 
}