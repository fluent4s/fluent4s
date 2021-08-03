package parser
import cats.parse.{Parser0, Parser => P, Numbers, Accumulator, Rfc5234}

object Ftl {
  /* whitespaces */
  private[this] val blank_inline: P[Unit] = P.char('\u0020').rep.void;
  private[this] val line_end: P[Unit] =
    (P.string("\u000D\u000A") orElse (P.char('\u000A'))).void;
  private[this] val end_of_file: Parser0[Unit] = P.end;
  private[this] val blank_block: P[Unit] =
    (blank_inline | line_end).rep.void;
  private[this] val blank: Parser0[Unit] =
    (blank_inline orElse line_end).rep.?.void <* end_of_file.?;

  /* digits */
  private[this] val digit: P[Char] = Numbers.digit;
  private[this] val digits: P[String] = Numbers.digits;
  private[this] val hex: P[Char] =
    P.charIn('0' to '9') orElse P.charIn('a' to 'f') orElse P.charIn(
      'A' to 'F'
    );

  /* string literals */
  private[this] val special_quoted_char: P[Char] = P.charIn(List('"', '\\'));
  private[this] val special_escape: P[Char] =
    (P.char('\\') *> special_quoted_char);
  private[this] val unicode_escape: P[String] =
    ((P.string("\\u") *> (hex.repExactlyAs(4)(
      Accumulator.nonEmptyListAccumulator0[Char]
    ))) orElse (P.string("\\U") *> (hex.repExactlyAs(6)(
      Accumulator.nonEmptyListAccumulator0[Char]
    )))).map(_.toList.mkString(""));
  private[this] val quoted_char: P[String] = any_char
    .map(_.toString)
    .filter(c =>
      special_quoted_char.parse(c).isLeft && line_end
        .parse(c)
        .isLeft && end_of_file.parse(c).isLeft
    ) | special_escape.map(_.toString) | unicode_escape;

  /* Text elements */
  private[this] val special_text_char: P[Unit] = P.char('{') | P.char('}');
  private[this] val text_char: P[Char] = any_char.filter(c =>
    special_text_char.parse(c.toString).isLeft && line_end
      .parse(c.toString)
      .isLeft && end_of_file.parse(c.toString).isLeft
  );
  private[this] val indented_char: P[Char] =
    text_char.filter(!List('[', '*', '.').contains(_));

  /* Content Characters */

  /* WARNING:
   *
   * Limitation of Java and thus of Scala, a Char can only be represented on 4 bytes, it is impossible to
   * represent a part of the Unicode table directly in the FTL files using this lib (U+00FFFF to U+10FFFF)
   * contrary to the specification of Project Fluent.
   * It is thus necessary to escape all the Unicode characters of this portion which could not be treated
   * by fluent-scala.
   *
   * See: https://github.com/projectfluent/fluent/blob/master/spec/fluent.ebnf#L87-L96
   */
  private[this] val any_char: P[Char] = P.charIn('\u0000' to '\uFFFF')

  /* Content Characters */
  private[this] val identifier: P[String] =
    (Rfc5234.alpha ~ (Rfc5234.alpha | digit | P.charIn(List('-', '_'))).rep0)
      .map((head, tail) => (head :: tail).mkString(""));

  /* Block Expressions */
  private[this] val variant_key: P[_] = (number_literal | identifier)
    .between(P.char('[') <* blank.?, blank.? *> P.char(']'));
  private[this] val variant =
    (line_end *> blank.? *> (variant_key <* blank_inline.?)) ~ P.defer(pattern);
  private[this] val default_variant =
    (line_end *> blank.? *> P.char('*') *> (variant_key <* blank_inline.?)) ~ P
      .defer(pattern);
  private[this] val variant_list =
    variant.rep0.with1 ~ default_variant ~ (variant.rep0 <* line_end);
  private[this] val select_expression =
    (P.defer(inline_expression) <* blank.? <* P.string(
      "->"
    ) <* blank_inline.?) ~ variant_list;

  /* Inline Expressions */
  private[this] val function_reference = identifier ~ call_argument;
  private[this] val message_reference: P[(String, Option[String])] =
    identifier ~ attributes_accessor.?;
  private[this] val term_reference =
    P.char('-') *> identifier ~ attributes_accessor.? ~ call_argument.?;
  private[this] val variable_reference: P[String] = P.char('$') *> identifier;
  private[this] val attributes_accessor: P[String] = P.char('.') *> identifier;
  private[this] val call_argument = argument_list.between(
    P.char('(').surroundedBy(blank.?),
    blank.? ~ P.char(')')
  );
  private[this] val argument_list =
    argument.repSep(P.char(',').surroundedBy(blank.?));
  private[this] val argument = named_argument | P.defer(inline_expression);
  private[this] val named_argument = identifier ~ P
    .char(':')
    .surroundedBy(blank.?) ~ (string_literal | number_literal);

  /* Literals */
  private[this] val string_literal: P[String] =
    (P.char('"') *> quoted_char.rep0 <* P.char('"')).map(_.mkString(""));
  private[this] val number_literal: P[(Boolean, Int, Option[Int])] =
    (P.char('-').?.with1 ~ digits ~ (P.char('.') *> digits).?).map((a, b) =>
      (a._1.isDefined, a._2.toInt, b.map(_.toInt))
    )

  /* Rules */
  private[this] val inline_expression = string_literal
  | number_literal
    | function_reference
    | message_reference
    | term_reference
    | variable_reference
    | inline_placeable

  /* TextElements & Placeable */
  private[this] val inline_placeable =
    (select_expression | P.defer(inline_expression))
      .between(P.char('{') ~ blank.?, blank.? ~ P.char('}'))
  private[this] val block_placeable =
    (blank_block ~ blank_inline.?).with1 ~ inline_placeable
  private[this] val inline_text: P[String] =
    text_char.rep.map(_.toList.mkString(""));
  private[this] val block_text: P[Option[String]] =
    blank_block.with1 *> blank_inline *> indented_char *> inline_text.?;
  private[this] val pattern_element =
    inline_text | block_text | inline_placeable | block_placeable

  /* Pattern */
  private[this] val pattern = pattern_element.rep;

  /* Attribute */
  private[this] val attribute = line_end *> blank.? *> P.char(
    '.'
  ) *> identifier <* P.char('=').surroundedBy(blank_inline.?) ~ pattern;

  /* Junk */
  private[this] val junk_line: P[String] =
    (P.until0(P.char('\n')).with1 <* P.char('\u000A'));
  private[this] val junk_eof: Parser0[String] =
    (P.until0(P.char('\n')) <* P.end);
  private[this] val junk: P[String] =
    (junk_line.repUntil(
      P.charIn(List('#', '-')) | Rfc5234.alpha
    ) ~ junk_eof.?).map(_ match {
      case (t1, Some(t2)) => t1.toList.mkString("") + t2
      case (t1, _)        => t1.toList.mkString("")
    });

  /* Comments */
  private[this] val comment_char: P[Char] =
    P.not(P.char('\n')).with1 *> any_char;
  private[this] val comment_line: P[Option[String]] =
    P.char('#').rep(1, 3) *> (P.char('\u0020') *> comment_char.rep0.map(
      _.toList.mkString("")
    )).? <* line_end;

  /* Entries */
  private[this] val message = identifier <* P
    .char('=')
    .surroundedBy(blank_inline.?) ~ (pattern ~ attribute.rep0 | attribute.rep);
  private[this] val term = P.char('-') *> identifier <* P
    .char('=')
    .surroundedBy(blank_inline.?) ~ pattern ~ attribute.rep0;
  private[this] val entry =
    (message ~ line_end) | (term ~ line_end) | comment_line;

  /* Resource */
  private[this] val resource: Parser0[_] = (entry | blank_block | junk)
    .repUntil0(junk_eof orElse P.end) ~ (junk_eof.? orElse P.end);
}
