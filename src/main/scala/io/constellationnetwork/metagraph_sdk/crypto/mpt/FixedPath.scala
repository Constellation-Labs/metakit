package io.constellationnetwork.metagraph_sdk.crypto.mpt

//package xyz.kd5ujc.accumulators.mpt
//
//import cats.data.Validated
//import cats.syntax.either._
//import cats.syntax.traverse._
//import cats.instances.list._
//
//import xyz.kd5ujc.hash.{Digest, l256, l512}
//
//import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
//import io.circe.syntax._
//
//sealed trait FixedPathError
//case class InvalidPathLength(actual: Int, expected: Int) extends FixedPathError
//case class InvalidNibbles(errors: List[InvalidNibble]) extends FixedPathError
//
///** A path in a Merkle Patricia Trie with a fixed length
//  */
//sealed trait FixedPath {
//  def nibbles: Seq[Nibble]
//  def startsWith(prefix: Seq[Nibble]): Boolean = nibbles.startsWith(prefix)
//  def drop(n: Int): Seq[Nibble] = nibbles.drop(n)
//  def head: Nibble = nibbles.head
//  def tail: Seq[Nibble] = nibbles.tail
//  override def toString: String = nibbles.mkString
//}
//
//object FixedPath {
//  private val Size256: Int = 32 // 256 bits = 32 bytes
//  private val Size512: Int = 64 // 512 bits = 64 bytes
//
//  case class Path256(override val nibbles: Seq[Nibble]) extends FixedPath
//  case class Path512(override val nibbles: Seq[Nibble]) extends FixedPath
//
//  /** Create a path from a digest
//    */
//  def fromDigest(digest: Digest): FixedPath = digest match {
//    case d: l256 => Path256(Nibble(d))
//    case d: l512 => Path512(Nibble(d))
//  }
//
//  /** Create a path from a sequence of nibbles with validation
//    */
//  def fromNibbles(nibbles: Seq[Nibble]): Validated[FixedPathError, FixedPath] = {
//    val size256 = Size256 * 2
//    val size512 = Size512 * 2
//    if (nibbles.length == size256)
//      Validated.valid(Path256(nibbles))
//    else if (nibbles.length == size512)
//      Validated.valid(Path512(nibbles))
//    else
//      Validated.invalid(InvalidPathLength(nibbles.length, InvalidPathLength(nibbles.length, size256).toString))
//  }
//
//  /** Create a path from a hex string with validation
//    */
//  def fromHexString(hex: String): Validated[FixedPathError, FixedPath] = {
//    import cats.instances.either._
//    import cats.instances.list._
//    import cats.syntax.either._
//
//    val validatedNibbles = hex.toList.traverse(c => Nibble.validated(c).toEither)
//    validatedNibbles.fold(
//      errors => Validated.invalid(InvalidNibbles(errors)),
//      nibbles => fromNibbles(nibbles)
//    )
//  }
//
//  implicit val fixedPathEncoder: Encoder[FixedPath] = {
//    case Path256(nib) => Json.obj(
//      "type" -> "256".asJson,
//      "nibbles" -> Json.fromValues(nib.map(n => Json.fromString(n.toString)))
//    )
//    case Path512(nib) => Json.obj(
//      "type" -> "512".asJson,
//      "nibbles" -> Json.fromValues(nib.map(n => Json.fromString(n.toString)))
//    )
//  }
//
//  implicit val fixedPathDecoder: Decoder[FixedPath] = (c: HCursor) =>
//    for {
//      pathType <- c.downField("type").as[String]
//      nibbles <- c.downField("nibbles").as[List[String]].flatMap { hexStrings =>
//        hexStrings.traverse(s =>
//          Nibble.validated(s.charAt(0))
//            .toEither
//            .leftMap(err => DecodingFailure(err.toString, c.history))
//        )
//      }
//      path <- fromNibbles(nibbles).toEither.leftMap(err => DecodingFailure(err.toString, c.history))
//      _ <- pathType match {
//        case "256" if !path.isInstanceOf[Path256] => Left(DecodingFailure("Expected Path256", c.history))
//        case "512" if !path.isInstanceOf[Path512] => Left(DecodingFailure("Expected Path512", c.history))
//        case _ => Right(())
//      }
//    } yield path
//}
