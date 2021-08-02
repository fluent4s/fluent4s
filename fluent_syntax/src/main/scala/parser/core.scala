package parser
import cats.parse.{Parser0, Parser => P, Numbers, Accumulator, Rfc5234}

object Ftl {
  /* whitespaces */
  private[this] val blank_inline: P[Unit] = P.char('\u0020').rep.void;
  private[this] val line_end: P[Unit] =
    (P.string("\u000D\u000A") orElse (P.char('\u000A'))).void;
  private[this] val end_of_file: Parser0[Unit] = P.end;
  private[this] val blank_block: Parser0[Unit] =
    ((blank_inline | line_end).rep.?.void <* end_of_file.?);
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
  private[this] val variant_key: Parser0[_] = (number_literal | identifier)
    .between(P.char('[') <* blank.?, blank.? *> P.char(']'));
  private[this] val variant = ??? /* TODO: need pattern */
  private[this] val default_variant = ??? /* TODO: need pattern */
  private[this] val variant_list = ??? /* TODO: need variant, default_variant */
  private[this] val select_expression =
    ??? /* TODO: need variant_list, inline_expression */

  /* Inline Expressions */
  private[this] val function_reference = ??? /* TODO: need call_argument */
  private[this] val message_reference: P[(String, Option[String])] =
    identifier ~ attributes_accessor.?;
  private[this] val term_reference = ??? /* TODO: need call_argument */
  private[this] val variable_reference: P[String] = P.char('$') *> identifier;
  private[this] val attributes_accessor: P[String] = P.char('.') *> identifier;
  private[this] val call_argument = ??? /* TODO: need argument_list */
  private[this] val argument_list = ??? /* TODO: need argument */
  private[this] val argument = ??? /* TODO: need inline_expression */
  private[this] val named_argument = identifier ~ P
    .char(':')
    .surroundedBy(blank.?) ~ (string_literal | number_literal);

  /* Literals */
  private[this] val string_literal: P[String] =
    (P.char('"') *> quoted_char.rep0 <* P.char('"')).map(_.mkString(""));
  private[this] val number_literal: Parser0[(Boolean, Int, Option[Int])] =
    (P.char('-').? ~ digits ~ (P.char('.') *> digits).?).map((a, b) =>
      (a._1.isDefined, a._2.toInt, b.map(_.toInt))
    )

  /* Rules */
  private[this] val inline_expression =
    ??? /* TODO: need function_reference, term_reference, inline_placeable */

  /* TextElements & Placeable */
  private[this] val inline_placeable =
    ??? /* TODO: need inline_expression, select_expression */
  private[this] val block_placeable = ??? /* TODO: need inline_placeable */
  private[this] val inline_text: P[String] =
    text_char.rep.map(_.toList.mkString(""));
  private[this] val block_text: Parser0[Option[String]] =
    blank_block *> blank_inline *> indented_char *> inline_text.?;
  private[this] val pattern_element =
    ??? /* TODO: need inline_placeable, block_placeable */

  /* Pattern */
  private[this] val pattern = ??? /* TODO: need pattern_element */

  /* Attribute */
  private[this] val attribute = ??? /* TODO: need pattern */

  /* Junk */
  private[this] val junk_line: Parser0[Unit] =
    (P.until0(P.char('\n')) ~ (P.char('\u000A') | P.end)).void;
  private[this] val junk: Parser0[Unit] =
    ??? /* WIP junk_line.repUntil0(P.charIn(List('#', '-')) | Rfc5234.alpha).void; */

}
