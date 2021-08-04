package fluent_syntax.unicode

import scala.io.Codec;

object Utf8 {
  final val UNKNWOWN_CHAR: Char = '�'
  def from_codepoint(code: Integer): String = {
    code match {
      case x if (0x0000 to 0x007f) contains x =>
        Codec.fromUTF8(Array(x.toByte)).mkString("")
      case x if (0x0080 to 0x07ff) contains x => {
        val first = 0xc0 | ((x & 0x7c0) >>> 6).toByte
        val second = 0x80 | (x & 0x3f).toByte
        Codec.fromUTF8(Array(first.toByte, second.toByte)).mkString("")
      }
      case x if (0x0800 to 0xffff) contains x => {
        val first = 0xe0 | ((x & 0xf000) >>> 12).toByte
        val second = 0x80 | ((x & 0xfc0) >>> 6).toByte
        val third = 0x80 | (x & 0x3f).toByte
        Codec
          .fromUTF8(Array(first.toByte, second.toByte, third.toByte))
          .mkString("")
      }
      case x if (0x10000 to 0x10ffff) contains x => {
        val first = 0xf0 | ((x & 0x1c0000) >>> 18).toByte
        val second = 0x80 | ((x & 0x3f000) >>> 12).toByte
        val third = 0x80 | ((x & 0xfc0) >>> 6).toByte
        val fourth = 0x80 | (x & 0x3f).toByte
        Codec
          .fromUTF8(
            Array(first.toByte, second.toByte, third.toByte, fourth.toByte)
          )
          .mkString("")
      }
      case _ => UNKNWOWN_CHAR.toString
    }
  }
}
