package io.constellationnetwork.metagraph_sdk.crypto.mpt

import cats.data.Validated
import cats.implicits.toTraverseOps
import cats.syntax.bifunctor._

import scala.collection.immutable.ArraySeq

import io.constellationnetwork.security.hash.Hash

import io.circe._
import io.circe.syntax.EncoderOps

class Nibble private (val value: Byte) extends AnyVal {

  override def toString: String = {
    val hexChars = "0123456789abcdef".toCharArray
    "" + hexChars(value & 0x0f)
  }
}

object Nibble {
  val empty: Nibble = new Nibble(0: Byte)

  private val hexChars: Array[Char] = "0123456789abcdef".toCharArray

  def apply(digest: Hash): Seq[Nibble] =
    digest.value.view.map(unsafe).to(ArraySeq)

  def apply(bytes: Array[Byte]): Seq[Nibble] =
    bytes.view.flatMap(apply).to(ArraySeq)

  def apply(byte: Byte): Seq[Nibble] =
    Seq(
      Nibble.unsafe((byte >> 4 & 0x0f).toByte),
      Nibble.unsafe((byte & 0x0f).toByte)
    )

  def fromHexString(hexString: String): Either[InvalidNibble, Seq[Nibble]] =
    hexString.toList.traverse(validated(_).toEither).map(_.to(ArraySeq))

  def unsafe(byte: Byte): Nibble = new Nibble(byte)

  def unsafe(char: Char): Nibble = char match {
    case c if c >= '0' && c <= '9' => Nibble.unsafe((c - '0').toByte)
    case c if c >= 'a' && c <= 'f' => Nibble.unsafe((c - 'a' + 10).toByte)
    case c if c >= 'A' && c <= 'F' => Nibble.unsafe((c - 'A' + 10).toByte)
    case _                         => throw new IllegalArgumentException("Invalid character: " + char)
  }

  def toBytes(nibbles: Seq[Nibble]): Array[Byte] =
    nibbles
      .grouped(2)
      .collect { case Seq(high, low) =>
        ((high.value << 4) | (low.value & 0x0f)).toByte
      }
      .toArray

  def validated(byte: Byte): Validated[InvalidNibble, Nibble] =
    if (byte >= 0 && byte <= 15) Validated.valid(new Nibble(byte))
    else Validated.invalid(ByteOutOfRange)

  def validated(c: Char): Validated[InvalidNibble, Nibble] = c match {
    case c if c >= '0' && c <= '9' => Validated.valid(Nibble.unsafe((c - '0').toByte))
    case c if c >= 'a' && c <= 'f' => Validated.valid(Nibble.unsafe((c - 'a' + 10).toByte))
    case c if c >= 'A' && c <= 'F' => Validated.valid(Nibble.unsafe((c - 'A' + 10).toByte))
    case _                         => Validated.invalid(CharOutOfRange)
  }

  def commonPrefix(a: Seq[Nibble], b: Seq[Nibble]): Seq[Nibble] =
    a.zip(b)
      .takeWhile(Function.tupled(_.value == _.value))
      .map(_._1)

  implicit val nibbleEncoder: Encoder[Nibble] = (a: Nibble) => Json.fromString("" + hexChars(a.value & 0x0f))

  implicit val nibbleDecoder: Decoder[Nibble] = (c: HCursor) =>
    c.as[String].flatMap { res =>
      if (res.length == 1) validated(res.charAt(0)).toEither.leftMap(err => DecodingFailure(err.toString, c.history))
      else Left(DecodingFailure("Nibble string must be of length 1", c.history))
    }

  implicit val nibbleSeqEncoder: Encoder[Seq[Nibble]] =
    (seq: Seq[Nibble]) => seq.map(a => hexChars(a.value & 0x0f)).mkString("").asJson

  implicit val nibbleSeqDecoder: Decoder[Seq[Nibble]] = (c: HCursor) =>
    c.as[String].flatMap { res =>
      res.toList.traverse(validated(_).toEither.leftMap(err => DecodingFailure(err.toString, c.history)))
    }

  implicit val nibbleKeyEncoder: KeyEncoder[Nibble] = (key: Nibble) => "" + hexChars(key.value & 0x0f)

  implicit val nibbleKeyDecoder: KeyDecoder[Nibble] = (key: String) =>
    if (key.length == 1) validated(key.charAt(0)).toOption else None
}

sealed trait InvalidNibble extends Throwable
case object ByteOutOfRange extends InvalidNibble
case object CharOutOfRange extends InvalidNibble
