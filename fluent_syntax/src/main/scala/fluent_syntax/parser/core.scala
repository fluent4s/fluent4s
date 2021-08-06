package fluent_syntax.parser
import cats.parse.{Parser0, Parser => P, Numbers, Accumulator, Rfc5234}
import fluent_syntax.ast._
import fluent_syntax.unicode.Utf8;
import cats.data.NonEmptyList

object Ftl {
  /* whitespaces */
  private[parser] val blank_inline: P[Unit] = P.char('\u0020').rep.void;
  private[parser] val line_end: P[Unit] =
    (P.string("\u000D\u000A").backtrack orElse (P.char('\u000A'))).void;
  private[parser] val end_of_file: Parser0[Unit] = P.end;
  private[parser] val blank_block: P[Unit] =
    (blank_inline.backtrack | line_end).rep.void;
  private[parser] val blank: Parser0[Unit] =
    (blank_inline.backtrack orElse line_end).rep0.void <* end_of_file.?;

  /* digits */
  private[parser] val digit: P[Char] = Numbers.digit;
  private[parser] val digits: P[String] = Numbers.digits;
  private[parser] val hex: P[Char] =
    P.charIn(
      List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd',
        'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F')
    )

  /* Content Characters */

  /* WARNING:
   *
   * Limitation of Java and thus of Scala, a Char can only be represented on 4 bytes, it is impossible to
   * represent a part of the Unicode table directly in the FTL files using parser lib (U+00FFFF to U+10FFFF)
   * contrary to the specification of Project Fluent.
   * It is thus necessary to escape all the Unicode characters of parser portion which could not be treated
   * by fluent-scala.
   *
   * See: https://github.com/projectfluent/fluent/blob/master/spec/fluent.ebnf#L87-L96
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
      .map(Utf8.from_codepoint(_));
  private[parser] val quoted_char: P[String] = ((P
    .not(special_quoted_char | P.end)
    .with1 *> any_char).backtrack | special_escape.backtrack)
    .map(_.toString) | unicode_escape

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
      case (Some(a: String), (b: String, Some(c: String, d: String))) =>
        a + b + c + d
      case (_, (b: String, Some(c: String, d: String))) => b + c + d
      case (Some(a: String), (b: String, _))            => a + b
      case (_, (b: String, _))                          => b
    })

  /* Identifier */
  private[parser] val identifier: P[FIdentifier] =
    (Rfc5234.alpha ~ (Rfc5234.alpha | digit | P.charIn(List('-', '_'))).rep0)
      .map((head, tail) => new FIdentifier((head :: tail).mkString("")));

  /* Block Expressions */
  private[parser] val variant_key: P[FVariantKey] = (number_literal.map(
    new NumberLiteralKey(_)
  ) | identifier.map(new IdentifierKey(_)))
    .between(P.char('[') <* blank.?, blank.? *> P.char(']'));
  private[parser] val variant: P[FVariant] =
    ((line_end *> blank.? *> (variant_key <* blank_inline.?)) ~ P.defer(
      pattern
    )).map({ case (key, value) => new FVariant(key, value, false) });
  private[parser] val default_variant: P[FVariant] =
    ((line_end *> blank.? *> P.char(
      '*'
    ) *> (variant_key <* blank_inline.backtrack.?)) ~ P
      .defer(pattern)).map({ case (key, value) =>
      new FVariant(key, value, true)
    });
  private[parser] val variant_list: P[List[FVariant]] =
    (variant.backtrack.rep0.with1 ~ (default_variant ~ (variant.backtrack.rep0 <* line_end)))
      .map({
        case (
              pre: List[FVariant],
              (default: FVariant, after: List[FVariant])
            ) =>
          pre ::: (default :: after)
      });
  private[parser] val select_expression: P[Select] =
    ((P.defer(inline_expression) <* blank.? <* P.string(
      "->"
    ) <* blank_inline.?) ~ variant_list).map(new Select(_, _));

  /* Inline Expressions */
  private[parser] val attributes_accessor: P[FIdentifier] =
    P.char('.') *> identifier;
  private[parser] val named_argument: P[NamedArgument] = ((identifier <* P
    .char(':')
    .surroundedBy(blank.?)) ~ (string_literal.map(
    new StringLiteral(_)
  ) | number_literal.map(new NumberLiteral(_)))).map(new NamedArgument(_, _));
  private[parser] val argument: P[FArgument] =
    named_argument.backtrack | P
      .defer(inline_expression)
      .map(new PositionalArgument(_));
  private[parser] val argument_list: Parser0[List[FArgument]] =
    argument.backtrack.repSep0(P.char(',').surroundedBy(blank.?));
  private[parser] val call_argument: P[FCallArguments] =
    (P.char('(').surroundedBy(blank.?) *> argument_list
      <* (blank.? ~ P.char(')')))
      .map(_.fold((List.empty[NamedArgument], List.empty[FInlineExpression])) {
        case (
              (named: List[NamedArgument], pos: List[FInlineExpression]),
              arg: FArgument
            ) =>
          arg match {
            case NamedArgument(name, value) =>
              (new NamedArgument(name, value) :: named, pos)
            case PositionalArgument(value) => (named, value :: pos)
          }
      })
      .map({ case (named: List[NamedArgument], pos: List[FInlineExpression]) =>
        new FCallArguments(pos, named)
      });
  private[parser] val function_reference: P[FunctionReference] =
    (identifier ~ call_argument)
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
  private[parser] def inline_expression: P[FInlineExpression] = string_literal
    .map(new StringLiteral(_))
    .orElse(number_literal.map(new NumberLiteral(_)))
    .orElse(function_reference)
    .orElse(message_reference)
    .orElse(term_reference)
    .orElse(variable_reference)
    .orElse(inline_placeable)

  /* TextElements & Placeable */
  private[parser] val inline_placeable: P[PlaceableExpr] =
    (select_expression | P.defer(inline_expression).map(new Inline(_)))
      .between(P.char('{') ~ blank.?, blank.? ~ P.char('}'))
      .map(new PlaceableExpr(_));
  private[parser] val block_placeable: P[PlaceableExpr] =
    (blank_block ~ blank_inline.?).with1 *> inline_placeable
  private[parser] val inline_text: P[String] =
    text_char.rep.backtrack.map(_.toList.mkString(""));
  private[parser] val block_text: P[String] =
    blank_block.with1 *> blank_inline *> indented_char *> inline_text.?.string;
  private[parser] val pattern_element: P[FPatternElement] =
    inline_text.backtrack
      .map(new TextElement(_))
      .orElse(block_text.backtrack.map(new TextElement(_)))
      .orElse(
        inline_placeable.backtrack.map(new Inline(_)).map(new Placeable(_))
      )
      .orElse(
        block_placeable.backtrack.map(new Inline(_)).map(new Placeable(_))
      )

  /* Pattern */
  private[parser] val pattern: P[FPattern] =
    pattern_element.rep.map({ case elements: NonEmptyList[FPatternElement] =>
      new FPattern(elements.toList)
    });

  /* Attribute */
  private[parser] val attribute: P[FAttribute] =
    ((line_end *> blank.? *> P.char(
      '.'
    ) *> identifier <* P.char('=').surroundedBy(blank_inline.?)) ~ pattern)
      .map({ case (id: FIdentifier, pat: FPattern) =>
        new FAttribute(id, pat)
      });

  /* Junk */
  private[parser] val junk_line: P[String] =
    (P.until0(P.char('\n')).with1 <* P.char('\u000A'));
  private[parser] val junk_eof: Parser0[String] =
    (P.until0(P.char('\n')) <* P.end);
  private[parser] val junk: P[Junk] =
    (junk_line.repUntil(
      P.charIn(List('#', '-')) | Rfc5234.alpha | P.end
    ) ~ junk_eof.?).map(_ match {
      case (t1, Some(t2)) => new Junk(t1.toList.mkString("") + t2)
      case (t1, _)        => new Junk(t1.toList.mkString(""))
    });

  /* Comments */
  private[parser] val comment_char: P[Char] =
    P.not(P.char('\n')).with1 *> any_char;
  private[parser] val comment_line: P[FEntry] =
    (P.char('#').rep(1, 3).string ~ (P.char('\u0020') *> comment_char.rep0.map(
      _.toList.mkString("")
    )).? <* line_end).map({
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
    .surroundedBy(blank_inline.?)) ~ (pattern ~ attribute.rep0 | attribute.rep
    .map(_.toList)))
    .map({
      case (id: FIdentifier, (value: FPattern, attrs: List[FAttribute])) =>
        new FMessage(id, Some(value), attrs, None)
      case (id: FIdentifier, attrs: List[FAttribute]) =>
        new FMessage(id, None, attrs, None)
    });
  private[parser] val term: P[FTerm] = (P.char('-') *> (identifier <* P
    .char('=')
    .surroundedBy(blank_inline.?)) ~ (pattern ~ attribute.rep0))
    .map({ case (id: FIdentifier, (value: FPattern, attrs: List[FAttribute])) =>
      new FTerm(id, value, attrs, None)
    });
  private[parser] val entry: P[FEntry] =
    (message.map(new Message(_)) <* line_end) | (term.map(
      new Term(_)
    ) <* line_end) | comment_line;

  /* Resource */
  private[parser] val resource: Parser0[_] = ((entry | blank_block | junk)
    .repUntil0(junk_eof orElse P.end)
    .map(_.filter(_.isInstanceOf[FEntry])) ~ (junk_eof
    .map(new Junk(_))
    .map(Some(_)) orElse P.end.as(Option.empty)))
    .map({
      case (entries: List[FEntry], Some(last: FEntry)) =>
        new FResource(last :: entries)
      case (entries: List[FEntry], None) => new FResource(entries)
    });
}
