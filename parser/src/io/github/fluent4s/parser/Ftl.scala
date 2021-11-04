package io.github.fluent4s.parser

import io.github.fluent4s.ast._
import cats.data.NonEmptyList
import cats.parse.{Accumulator, Numbers, Parser => P, Parser0, Rfc5234}

object Ftl {

  /* whitespaces */
  private[parser] val blank_inline: P[Unit] = P.char('\u0020').rep.void;
  private[parser] val line_end: P[Unit] =
    (P.string("\u000D\u000A")
      .backtrack orElse (P.char('\u000A').backtrack)).void;
  private[parser] val end_of_file: Parser0[Unit] = P.end;
  private[parser] val blank_block: P[Unit] =
    (blank_inline.backtrack.?.with1 *> line_end.backtrack).rep.void;
  private[parser] val blank: Parser0[Unit] =
    P.oneOf(List(line_end.backtrack, blank_inline.backtrack)).rep0.void;

  /* digits */
  private[parser] val digit: P[Char] = Numbers.digit;
  private[parser] val digits: P[String] = Numbers.digits;
  private[parser] val hex: P[Char] =
    P.charIn(
      List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B',
        'C', 'D', 'E', 'F')
    )

  /* Content Characters */

  /* WARNING:
   *
   * Limitation of Java and thus of Scala, a Char can only be represented on 4 bytes, it is impossible to
   * represent a part of the Unicode table directly using Char, so it split the character in multiple Char object.
   * It should be compatible with all UTF-8 characters already supported by the Fluent Project.
   */
  private[parser] val any_char: P[Char] = P.charIn('\u0000' to '\uFFFF')

  /* string literals */
  private[parser] val special_quoted_char: P[Char] = P.charIn(List('"', '\\'));
  private[parser] val special_escape: P[Char] =
    (P.char('\\') *> special_quoted_char);
  private[parser] val unicode_escape: P[String] =
    ((P.string("\\u") *> (hex.repExactlyAs(4)(
      Accumulator.nonEmptyListAccumulator0[Char]
    ))) orElse (P.string("\\U") *> (hex.repExactlyAs(6)(
      Accumulator.nonEmptyListAccumulator0[Char]
    ))))
      .map(_.toList.mkString(""))
      .map(Integer.parseInt(_, 16))
      .map(Character.toChars(_).mkString(""));
  private[parser] val quoted_char: P[String] = ((P
    .not(special_quoted_char | line_end | P.end)
    .with1 *> any_char).backtrack | special_escape.backtrack)
    .map(_.toString) | unicode_escape.backtrack

  /* Text elements */
  private[parser] val special_text_char: P[Unit] = P.char('{') | P.char('}');
  private[parser] val text_char: P[Char] =
    P.not(special_text_char | line_end).with1 *> any_char;
  private[parser] val indented_char: P[Char] =
    P.not(P.charIn(List('[', '*', '.'))).with1 *> text_char;

  /* Literals */
  private[parser] val string_literal: P[String] =
    (P.char('"') *> quoted_char.rep0 <* P.char('"')).map(_.mkString(""));
  private[parser] val number_literal: P[String] =
    (P.char('-').string.?.with1 ~ (digits.string ~ (P
      .char('.')
      .string ~ digits.string).?)).map({
      case (Some(a: String), (b: String, Some((c: String, d: String)))) =>
        a + b + c + d
      case (_, (b: String, Some((c: String, d: String)))) => b + c + d
      case (Some(a: String), (b: String, _)) => a + b
      case (_, (b: String, _)) => b
    })

  /* Identifier */
  private[parser] val identifier: P[FIdentifier] =
    (Rfc5234.alpha ~ (Rfc5234.alpha | digit | P.charIn(List('-', '_'))).rep0).backtrack
      .map { case (head, tail) => new FIdentifier((head :: tail).mkString("")) };

  /* Block Expressions */
  private[parser] val variant_key: P[FVariantKey] =
    P.char('[').backtrack.soft *> blank *> (number_literal.map(
      new NumberLiteralKey(_)
    ) | identifier.map(new IdentifierKey(_))) <* (blank <* P.char(']'))
  private[parser] val variant: P[FVariant] =
    ((line_end.backtrack.soft *> blank_inline.? *> P.not(
      P.char('*')
    ) *> (variant_key.backtrack <* blank_inline.backtrack.?)) ~ P
      .defer(
        pattern.backtrack
      )
      .backtrack).map({ case (key, value) => new FVariant(key, value, false) });
  private[parser] val default_variant: P[FVariant] =
    ((line_end.backtrack.soft *> blank_inline.? *> P
      .char(
        '*'
      )
      .backtrack *> (variant_key.backtrack <* blank_inline.backtrack.?)) ~ P
      .defer(pattern.backtrack)
      .backtrack).map({ case (key, value) =>
      new FVariant(key, value, true)
    });
  private[parser] val variant_list: P[List[FVariant]] =
    (((variant.backtrack.rep0.with1 ~ default_variant.backtrack) ~ (variant.backtrack.rep0)) <* line_end.backtrack.?)
      .map({
        case (
              (pre: List[FVariant], default),
              after: List[FVariant]
            ) =>
          pre ::: (default :: after)
      });
  private[parser] val select_expression: P[Select] =
    ((P.defer(
      inline_expression
      .withContext("Message value, Message attribute, Term value and nested expression (select/inline) are not valid selector.")
      .mapFilter({
        case MessageReference(_,_) => None
        case TermReference(_, None, _) => None
        case PlaceableExpr(_) => None
        case other => Some(other)
      })
    ).backtrack.soft <* blank <* P.string(
        "->"
    ) <* blank_inline.backtrack.?) ~ variant_list).map { case (a, b) => new Select(a, b) };

  /* Inline Expressions */
  private[parser] val attributes_accessor: P[FIdentifier] =
    P.char('.') *> identifier;
  private[parser] val named_argument: P[NamedArgument] = ((identifier <* P
    .char(':')
    .surroundedBy(blank)) ~ (string_literal.map(
    new StringLiteral(_)
  ) | number_literal.map(new NumberLiteral(_)))).map { case (a, b) => new NamedArgument(a, b) };
  private[parser] val argument: P[FArgument] =
    named_argument.backtrack | P
      .defer(inline_expression)
      .map(new PositionalArgument(_));
  private[parser] val argument_list: Parser0[List[FArgument]] =
    argument.backtrack.repSep0(P.char(',').surroundedBy(blank));
  private[parser] val call_argument: P[FCallArguments] =
    (P.char('(').surroundedBy(blank).backtrack.soft *> argument_list
      <* (blank ~ P.char(')')))
      .map(_.fold((List.empty[NamedArgument], List.empty[FInlineExpression], true)) {
        case (
              (named: List[NamedArgument], pos: List[FInlineExpression], valid: Boolean),
              arg: FArgument
            ) =>
          arg match {
            case NamedArgument(name, value) => (new NamedArgument(name, value) :: named, pos, valid)
            case PositionalArgument(value) => {
              named.isEmpty match {
                case true => (named, value :: pos, valid)
                case false => (named, value :: pos, false)
              }
            }
          }
      })
      .withContext("Positional arg must not follow keyword args.")
      .mapFilter({
        case (named: List[NamedArgument], pos: List[FInlineExpression], true) => Some((named, pos)) 
        case (_: List[NamedArgument], _: List[FInlineExpression], false) => None
      })
      .withContext("Named arguments must be unique.")
      .mapFilter({
        case (named: List[NamedArgument], pos: List[FInlineExpression]) => {
          val unique = named.map(_.name).toSet.knownSize;
          (named.length == unique) match {
            case true => Some((named, pos))
            case false => None
          }
        }
      })
      .map({ case (named: List[NamedArgument], pos: List[FInlineExpression]) =>
        new FCallArguments(pos, named)
      });

  private[parser] val function_reference: P[FunctionReference] =
    (identifier.withContext("Function identifier should be in UPPERCASE.").mapFilter({ case id => id.name.forall(_.isUpper) match {
      case true => Some(id)
      case false => None
    }}).backtrack ~ call_argument)
      .map({ case (id: FIdentifier, arguments: FCallArguments) =>
        new FunctionReference(id, arguments)
      });
  private[parser] val message_reference: P[MessageReference] =
    (identifier ~ attributes_accessor.?).map({
      case (id: FIdentifier, attribute: Option[FIdentifier]) =>
        new MessageReference(id, attribute)
    });
  private[parser] val term_reference: P[TermReference] =
    ((P.char('-') *> identifier) ~ (attributes_accessor.? ~ call_argument.?))
      .map({
        case (
              id: FIdentifier,
              (
                attribute: Option[FIdentifier],
                arguments: Option[FCallArguments]
              )
            ) =>
          new TermReference(id, attribute, arguments)
      });
  private[parser] val variable_reference: P[VariableReference] =
    P.char('$') *> identifier.map(new VariableReference(_));

  /* Rules */
  private[parser] def inline_expression: P[FInlineExpression] =
    string_literal.backtrack
      .map(new StringLiteral(_))
      .orElse(number_literal.backtrack.map(new NumberLiteral(_)))
      .orElse(function_reference.backtrack)
      .orElse(message_reference.backtrack)
      .orElse(term_reference.backtrack)
      .orElse(variable_reference.backtrack)
      .orElse(inline_placeable.backtrack)

  /* TextElements & Placeable */
  private[parser] def inline_placeable: P[PlaceableExpr] =
    (select_expression
      .between(P.char('{') ~ blank, blank.? ~ P.char('}'))
      .backtrack orElse P
      .defer(inline_expression
        .withContext("Term attributes may not be used in Placeables.")
        .mapFilter({
          case TermReference(_, Some(_), _) => None
          case other => Some((other))
      }))
      .between(P.char('{') ~ blank, blank.? ~ P.char('}'))
      .backtrack
      .map(new Inline(_)))
      .map(new PlaceableExpr(_));
  private[parser] val block_placeable: P[PlaceableExpr] =
    ((blank_inline.backtrack.soft ~ line_end).backtrack orElse line_end).rep.void *> blank_inline.? *> inline_placeable.backtrack
  private[parser] val inline_text: P[String] =
    text_char.backtrack.rep.map(_.toList.mkString(""));
  private[parser] val block_text: P[BlockTextElement] =
    ((((blank_inline.backtrack.soft ~ line_end).backtrack orElse line_end).rep.void *> (P.index ~ (blank_inline *> P.index))
      .map({ case (a, b) => b - a })) ~ (indented_char.backtrack ~ inline_text.?).backtrack
      .map({
        case (a, Some(b)) => s"$a$b"
        case (a, None) => s"$a"
      })
      .?).map({ case (indent, text) =>
      new BlockTextElement(indent, text)
    })
  private[parser] val pattern_element: P[FPatternElement] =
    inline_placeable.backtrack
      .map(new Inline(_))
      .map(new Placeable(_))
      .orElse(
        block_placeable.backtrack
          .map(new Inline(_))
          .map(new Placeable(_))
      )
      .orElse(
        block_text.backtrack
      )
      .orElse(inline_text.backtrack.map(new TextElement(_)))

  /* Pattern */
  private[parser] val pattern: P[FPattern] =
    /* repUntil(block_text *> inline_text)? There is a special case we need to handle, a BlockText
     * followed by an InlineText is an invalid pattern, it generally mean, we were able to consume
     * some blank lines but we failed due to an indented char we cannot parse using a BlockText.
     * Therefore this indented char is then parsed as an InlineText, it should not! Never!
     */
    pattern_element.backtrack
      .repUntil((block_text *> inline_text).backtrack)
      .map({ case elements: NonEmptyList[FPatternElement] =>
        new FPattern(elements.toList)
      });

  /* Attribute */
  private[parser] val attribute: P[FAttribute] =
    ((line_end *> blank *> P.char(
      '.'
    ) *> identifier <* P.char('=').surroundedBy(blank_inline.?)) ~ pattern)
      .map({ case (id: FIdentifier, pat: FPattern) =>
        new FAttribute(id, pat)
      });

  /* Junk */
  private[parser] val junk_line: P[String] =
    (P.until0(P.char('\n')).with1 <* P.char('\n'));
  private[parser] val junk_eof: Parser0[String] =
    (P.until0(P.end) <* P.end);
  private[parser] val junk: P[Junk] =
    (junk_line ~ junk_line.repUntil0(P.oneOf(List(Rfc5234.alpha, P.charIn('-', '#')))))
      .map({
        case (junk1, junk2) => new Junk(junk1 + junk2.toList.mkString(""))
      })

  /* Comments */
  private[parser] val comment_char: P[Char] =
    P.not(P.char('\n')).with1 *> any_char;
  private[parser] val comment_line: P[FEntry] =
    (P.char('#').rep(1, 3).string ~ (P.char(' ') *> comment_char.rep0.map(
      _.toList.mkString("")
    )).?).map({
      case (prefix, comment) => {
        val com = new FComment(comment.getOrElse(""));
        prefix.length match {
          case 1 => new Comment(com)
          case 2 => new GroupComment(com)
          case 3 => new ResourceComment(com)
        }
      }
    });

  /* Entries */
  private[parser] val message: P[FMessage] = ((identifier <* P
    .char('=')
    .surroundedBy(
      blank_inline.?
    )).backtrack ~ (pattern ~ attribute.backtrack.rep0 | attribute.backtrack.rep
    .map(_.toList)))
    .map({
      case (id: FIdentifier, (value: FPattern, attrs: List[FAttribute])) =>
        new FMessage(id, Some(value), attrs, None)
      case (id: FIdentifier, attrs: List[FAttribute]) =>
        new FMessage(id, None, attrs, None)
    });
  private[parser] val term: P[FTerm] = (P.char('-') *> (identifier <* P
    .char('=')
    .surroundedBy(blank_inline.?)) ~ (pattern ~ attribute.backtrack.rep0))
    .map({ case (id: FIdentifier, (value: FPattern, attrs: List[FAttribute])) =>
      new FTerm(id, value, attrs, None)
    });
  private[parser] val entry: P[FEntry] =
    ((message.map(new Message(_))) orElse (term.map(
      new Term(_)
    )) orElse comment_line).backtrack <* (line_end | (line_end.backtrack ~ P.end) | P.end);

  /* Resource */
  private[parser] val resource: Parser0[FResource] =
    ((entry.backtrack orElse blank_block.backtrack orElse (junk.backtrack))
      .rep0
      .map(
        _.filter(_.isInstanceOf[FEntry]).map(_.asInstanceOf[FEntry])
      ) ~ junk_eof.map( 
      new Junk(_)
    ).?).map({
      case (entries, Some(last)) => new FResource(entries :+ last)
      case (entries, None) => new FResource(entries)
    });

  def parse(ftl: String): Either[P.Error, FResource] = resource.parseAll(ftl);

}
