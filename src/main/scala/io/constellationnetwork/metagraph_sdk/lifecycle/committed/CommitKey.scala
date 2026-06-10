package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import java.nio.charset.StandardCharsets

import cats.syntax.either._

import io.constellationnetwork.security.hex.Hex

import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

/**
 * A validated, namespaced path into the committed state dictionary -- the MPT key universe.
 *
 * Grammar (see `docs/committed-namespaces.md` for the full spec):
 *   - `key = segment *( "/" segment )`, 1 to [[CommitKey.MaxSegments]] segments
 *   - `segment = [a-z0-9] [a-z0-9._-]{0,63}` (lowercase only; no empty segments)
 *   - total length at most [[CommitKey.MaxKeyLength]] characters
 *
 * The MPT path of a key is the lowercase hex encoding of its UTF-8 bytes ([[toHex]]). Because the
 * encoding is byte-aligned and '/' is a single byte (0x2f), the hex of `"ns/"` is a strict prefix of
 * the hex of every key under namespace `ns` -- which is what makes namespace prefix proofs work.
 */
final case class CommitKey private (value: String) {

  /** The MPT path: lowercase hex of the UTF-8 bytes of [[value]]. */
  def toHex: Hex = CommitKey.hexOf(value)

  def segments: List[String] = value.split('/').toList

  /** The top-level namespace (first segment). */
  def namespace: String = segments.head
}

object CommitKey {
  val MaxSegmentLength: Int = 64
  val MaxSegments: Int = 16
  val MaxKeyLength: Int = 256

  private val SegmentPattern = "^[a-z0-9][a-z0-9._-]*$".r

  def from(value: String): Either[CommitKeyError, CommitKey] =
    validateSegments(value).map(_ => CommitKey(value))

  def unsafe(value: String): CommitKey =
    from(value).valueOr(throw _)

  private[committed] def hexOf(s: String): Hex =
    Hex.fromBytes(s.getBytes(StandardCharsets.UTF_8))

  private[committed] def validateSegments(value: String): Either[CommitKeyError, Unit] =
    if (value.isEmpty) CommitKeyError.EmptyKey.asLeft
    else if (value.length > MaxKeyLength) CommitKeyError.KeyTooLong(value.length).asLeft
    else {
      val segments = value.split('/').toList
      if (value.startsWith("/") || value.endsWith("/") || segments.exists(_.isEmpty))
        CommitKeyError.EmptySegment(value).asLeft
      else if (segments.length > MaxSegments) CommitKeyError.TooManySegments(segments.length).asLeft
      else
        segments.collectFirst {
          case s if s.length > MaxSegmentLength    => CommitKeyError.SegmentTooLong(s)
          case s if !SegmentPattern.matches(s)     => CommitKeyError.InvalidSegment(s)
        } match {
          case Some(err) => err.asLeft
          case None      => ().asRight
        }
    }

  implicit val ordering: Ordering[CommitKey] = Ordering.by(_.value)

  implicit val encoder: Encoder[CommitKey] = Encoder.encodeString.contramap(_.value)
  implicit val decoder: Decoder[CommitKey] = Decoder.decodeString.emap(s => from(s).leftMap(_.getMessage))
  implicit val keyEncoder: KeyEncoder[CommitKey] = KeyEncoder.instance(_.value)
  implicit val keyDecoder: KeyDecoder[CommitKey] = KeyDecoder.instance(s => from(s).toOption)
}

/**
 * A validated namespace -- a key prefix at segment granularity (same segment grammar as
 * [[CommitKey]], but it denotes the subtree of all keys strictly under `value + "/"`).
 */
final case class CommitNamespace private (value: String) {

  /**
   * The MPT path prefix covering every key strictly under this namespace: hex of the UTF-8 bytes of
   * `value + "/"`. The trailing separator pins the match to a segment boundary (so namespace
   * `fiber` cannot match a key under `fiberx/`).
   */
  def prefixHex: Hex = CommitKey.hexOf(value + "/")
}

object CommitNamespace {

  def from(value: String): Either[CommitKeyError, CommitNamespace] =
    CommitKey.validateSegments(value).map(_ => CommitNamespace(value))

  def unsafe(value: String): CommitNamespace =
    from(value).valueOr(throw _)
}

sealed abstract class CommitKeyError(message: String) extends RuntimeException(message)

object CommitKeyError {
  case object EmptyKey extends CommitKeyError("commit key must not be empty")
  final case class KeyTooLong(length: Int) extends CommitKeyError(s"commit key exceeds ${CommitKey.MaxKeyLength} chars: $length")
  final case class TooManySegments(count: Int) extends CommitKeyError(s"commit key exceeds ${CommitKey.MaxSegments} segments: $count")
  final case class EmptySegment(key: String) extends CommitKeyError(s"commit key has an empty segment: '$key'")
  final case class SegmentTooLong(segment: String)
      extends CommitKeyError(s"commit key segment exceeds ${CommitKey.MaxSegmentLength} chars: '$segment'")

  final case class InvalidSegment(segment: String)
      extends CommitKeyError(s"commit key segment must match ^[a-z0-9][a-z0-9._-]*$$: '$segment'")
}
