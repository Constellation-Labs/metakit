/*
 *  Based on Java implementation by WebPKI.org (http://webpki.org)
 *  Original Java source: https://github.com/webpki/json-canonicalization
 *
 *  This is a Scala port of the RFC 8785 canonicalization code, based on the Java
 *  implementation which was licensed under Apache 2.0.
 *
 *  Changes from original:
 *  - Made effect-polymorphic using cats-effect MonadThrow
 *  - Uses circe for JSON parsing
 *  - Functional approach with immutable data structures
 */
package io.constellationnetwork.metagraph_sdk.std

import java.io.IOException

import cats.MonadThrow
import cats.syntax.all._

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import io.circe._
import io.circe.syntax.EncoderOps

trait JsonCanonicalizer[F[_]] {
  def canonicalize[A: Encoder](content: A): F[String]
}

object JsonCanonicalizer {

  /**
   * JSON Canonicalization following RFC 8785
   * https://datatracker.ietf.org/doc/html/rfc8785
   */
  def make[F[_]: MonadThrow]: JsonCanonicalizer[F] = new JsonCanonicalizer[F] {

    private def escapeChar(c: Char): String = c match {
      case '\n'          => "\\n"
      case '\b'          => "\\b"
      case '\f'          => "\\f"
      case '\r'          => "\\r"
      case '\t'          => "\\t"
      case '"'           => "\\\""
      case '\\'          => "\\\\"
      case c if c < 0x20 => f"\\u${c.toInt}%04x"
      case c             => c.toString
    }

    private def serializeString(value: String): String = {
      val escaped = value.foldLeft("") { (acc, c) =>
        acc + escapeChar(c)
      }
      s"\"$escaped\""
    }

    private def encode(json: Json): F[String] = {
      sealed trait EncodeState
      case class ArrayState(remaining: List[Json], acc: List[String]) extends EncodeState
      case class ObjectState(remaining: List[(String, Json)], acc: List[String]) extends EncodeState
      case class Done(result: String) extends EncodeState

      def step(state: EncodeState): F[Either[EncodeState, String]] = state match {
        case ArrayState(Nil, acc) =>
          s"[${acc.reverse.mkString(",")}]".asRight[EncodeState].pure[F]

        case ArrayState(h :: t, acc) =>
          loop(h).map(value => Left(ArrayState(t, value :: acc)))

        case ObjectState(Nil, acc) =>
          s"{${acc.reverse.mkString(",")}}".asRight[EncodeState].pure[F]

        case ObjectState((key, value) :: t, acc) =>
          loop(value).map(v => Left(ObjectState(t, s"${serializeString(key)}:$v" :: acc)))

        case Done(result) =>
          result.asRight[EncodeState].pure[F]
      }

      def loop(json: Json): F[String] = json.fold(
        "null".pure[F],
        b => b.toString.pure[F],
        num => NumberToJson.serializeNumber(num.toDouble),
        str => serializeString(str).pure[F],
        arr => MonadThrow[F].tailRecM(ArrayState(arr.toList, Nil): EncodeState)(step),
        obj => MonadThrow[F].tailRecM(ObjectState(TreeOrderedMap.from(obj.toMap).toList, Nil): EncodeState)(step)
      )

      loop(json)
    }

    def canonicalize[A: Encoder](content: A): F[String] =
      encode(content.asJson)
  }

  implicit class JsonPrinterEncodeOps[F[_], A](private val _v: A) extends AnyVal {

    def toCanonical(implicit mt: MonadThrow[F], enc: Encoder[A]): F[String] =
      make[F].canonicalize(_v.asJson)
  }
}

private object NumberToJson {

  def serializeNumber[F[_]: MonadThrow](value: Double): F[String] =
    if (value == 0.0) "0".pure[F]
    else if (value.isNaN || value.isInfinite) new IOException("NaN/Infinity not allowed in JSON").raiseError
    else DoubleCoreSerializer.serialize(value)
}

private object TreeOrderedMap {

  def from[V](map: Map[String, V]): SortedMap[String, V] = {
    implicit val ordering: Ordering[String] = new Ordering[String] {
      def compare(a: String, b: String): Int = {
        val aBytes = a.getBytes("UTF-16BE")
        val bBytes = b.getBytes("UTF-16BE")
        val minLength = aBytes.length min bBytes.length

        LazyList
          .range(0, minLength)
          .map(i => (aBytes(i) & 0xff) - (bBytes(i) & 0xff))
          .find(_ != 0)
          .getOrElse(aBytes.length - bBytes.length)
      }
    }

    SortedMap.empty[String, V](ordering) ++ map
  }
}

object DoubleCoreSerializer {

  def serialize[F[_]: MonadThrow](value: Double): F[String] =
    DoubleSerializerImpl.make[F].serialize(value, ecmaMode = true)
}

/**
 * An implementation of Ryu for serializing IEEE-754 double precision values as specified by ECMAScript
 */
private class DoubleSerializerImpl[F[_]: MonadThrow] {
  import DoubleSerializerImpl._

  private def pow5bits(e: Int): F[Int] = MonadThrow[F].pure(((e * 1217359) >>> 19) + 1)

  @tailrec
  private def pow5Factor(value: Long, count: Int = 0): F[Int] =
    value match {
      case _ if value % 5 != 0   => count.pure[F]
      case _ if value % 25 != 0  => (count + 1).pure[F]
      case _ if value % 125 != 0 => (count + 2).pure[F]
      case _ if value % 625 != 0 => (count + 3).pure[F]
      case v =>
        val nextValue = v / 625
        if (nextValue <= 0) new IllegalArgumentException(s"Invalid value: $value").raiseError[F, Int]
        else pow5Factor(nextValue, count + 4)
    }

  private def multipleOfPowerOf5(value: Long, q: Int): F[Boolean] =
    pow5Factor(value).map(_ >= q)

  private def decimalLength(v: Long): F[Int] = MonadThrow[F].pure {
    if (v >= 1000000000000000000L) 19
    else if (v >= 100000000000000000L) 18
    else if (v >= 10000000000000000L) 17
    else if (v >= 1000000000000000L) 16
    else if (v >= 100000000000000L) 15
    else if (v >= 10000000000000L) 14
    else if (v >= 1000000000000L) 13
    else if (v >= 100000000000L) 12
    else if (v >= 10000000000L) 11
    else if (v >= 1000000000L) 10
    else if (v >= 100000000L) 9
    else if (v >= 10000000L) 8
    else if (v >= 1000000L) 7
    else if (v >= 100000L) 6
    else if (v >= 10000L) 5
    else if (v >= 1000L) 4
    else if (v >= 100L) 3
    else if (v >= 10L) 2
    else 1
  }

  private def mulPow5divPow2(m: Long, i: Int, j: Int): F[Long] = MonadThrow[F].pure {
    val mHigh = m >>> 31
    val mLow = m & 0x7fffffffL
    val bits13 = mHigh * pow5Split(i)(0) // 124
    val bits03 = mLow * pow5Split(i)(0) // 93
    val bits12 = mHigh * pow5Split(i)(1) // 93
    val bits02 = mLow * pow5Split(i)(1) // 62
    val bits11 = mHigh * pow5Split(i)(2) // 62
    val bits01 = mLow * pow5Split(i)(2) // 31
    val bits10 = mHigh * pow5Split(i)(3) // 31
    val bits00 = mLow * pow5Split(i)(3) // 0

    val actualShift = j - 3 * 31 - 21
    require(actualShift >= 0, s"Invalid shift: $actualShift")

    ((((((((bits00 >>> 31) + bits01 + bits10) >>> 31) + bits02 + bits11) >>> 31)
    + bits03 + bits12) >>> 21) + (bits13 << 10)) >>> actualShift
  }

  private def mulPow5InvDivPow2(m: Long, i: Int, j: Int): F[Long] = MonadThrow[F].pure {
    val mHigh = m >>> 31
    val mLow = m & 0x7fffffffL
    val bits13 = mHigh * pow5InvSplit(i)(0)
    val bits03 = mLow * pow5InvSplit(i)(0)
    val bits12 = mHigh * pow5InvSplit(i)(1)
    val bits02 = mLow * pow5InvSplit(i)(1)
    val bits11 = mHigh * pow5InvSplit(i)(2)
    val bits01 = mLow * pow5InvSplit(i)(2)
    val bits10 = mHigh * pow5InvSplit(i)(3)
    val bits00 = mLow * pow5InvSplit(i)(3)

    val actualShift = j - 3 * 31 - 21
    require(actualShift >= 0, s"Invalid shift: $actualShift")

    ((((((((bits00 >>> 31) + bits01 + bits10) >>> 31) + bits02 + bits11) >>> 31)
    + bits03 + bits12) >>> 21) + (bits13 << 10)) >>> actualShift
  }

  def serialize(value: Double, ecmaMode: Boolean): F[String] = {
    case class FloatComponents(
      e2:      Int,
      m2:      Long,
      sign:    Boolean,
      even:    Boolean,
      mv:      Long,
      mp:      Long,
      mm:      Long,
      mmShift: Int
    )

    case class DecimalBase(
      dv:                Long,
      dp:                Long,
      dm:                Long,
      e10:               Int,
      dmIsTrailingZeros: Boolean,
      dvIsTrailingZeros: Boolean
    )

    case class ZeroState(
      dp:                Long,
      dv:                Long,
      dm:                Long,
      removed:           Int,
      dmIsTrailingZeros: Boolean,
      dvIsTrailingZeros: Boolean,
      lastRemovedDigit:  Int
    )

    // Step 1: Decode IEEE-754 bits
    val bits = java.lang.Double.doubleToLongBits(value)
    val ieeeExponent = ((bits >>> DoubleMantissaBits) & DoubleExponentMask).toInt
    val ieeeMantissa = bits & DoubleMantissaMask

    val components = FloatComponents(
      e2 =
        if (ieeeExponent == 0) 1 - DoubleExponentBias - DoubleMantissaBits
        else ieeeExponent - DoubleExponentBias - DoubleMantissaBits,
      m2 =
        if (ieeeExponent == 0) ieeeMantissa
        else ieeeMantissa | (1L << DoubleMantissaBits),
      sign = bits < 0,
      even = false,
      mv = 0L,
      mp = 0L,
      mm = 0L,
      mmShift = 0
    ).pure[F]

    // Step 2: Determine interval
    def computeInterval(c: FloatComponents): F[FloatComponents] = {
      val interval = c.copy(
        even = (c.m2 & 1) == 0,
        mv = 4 * c.m2,
        mp = 4 * c.m2 + 2,
        mmShift = if ((c.m2 != (1L << DoubleMantissaBits)) || (ieeeExponent <= 1)) 1 else 0
      )

      interval
        .copy(
          mm = 4 * interval.m2 - 1 - interval.mmShift
        )
        .pure[F]
    }

    // Step 3: Convert to decimal power base
    def computeDecimalBase(c: FloatComponents): F[DecimalBase] = {
      val e2Adj = c.e2 - 2

      if (e2Adj >= 0) {
        val q = math.max(0, ((e2Adj * 78913) >>> 18) - 1)

        for {
          pow5q <- pow5bits(q)
          k = Pow5InvBitCount + pow5q - 1
          i = -e2Adj + q + k
          dv <- mulPow5InvDivPow2(c.mv, q, i)
          dp <- mulPow5InvDivPow2(c.mp, q, i)
          dm <- mulPow5InvDivPow2(c.mm, q, i)
          dvz <-
            if (q <= 21) {
              if (c.mv % 5 == 0) multipleOfPowerOf5(c.mv, q)
              else false.pure[F]
            } else false.pure[F]
          dmz <-
            if (q <= 21 && c.even) multipleOfPowerOf5(c.mm, q)
            else false.pure[F]
        } yield DecimalBase(dv, dp, dm, q, dmz, dvz)
      } else {
        val q = math.max(0, ((-e2Adj * 732923) >>> 20) - 1)
        val i = -e2Adj - q

        for {
          pow5i <- pow5bits(i)
          k = pow5i - Pow5BitCount
          j = q - k
          dv <- mulPow5divPow2(c.mv, i, j)
          dp <- mulPow5divPow2(c.mp, i, j)
          dm <- mulPow5divPow2(c.mm, i, j)
        } yield {
          val dvz =
            if (q <= 1) true
            else if (q < 63) (c.mv & ((1L << (q - 1)) - 1)) == 0
            else false
          val dmz = q <= 1 && c.even && c.mmShift == 1
          DecimalBase(dv, dp, dm, q + e2Adj, dmz, dvz)
        }
      }
    }

    // Step 4: Find shortest decimal representation
    def computeOutput(db: DecimalBase, c: FloatComponents): F[String] = {
      @tailrec
      def removeTrailingZeros(state: ZeroState, vplen: Int): (Long, Int, Int) =
        if (state.dp / 10 > state.dm / 10) {
          removeTrailingZeros(
            ZeroState(
              dp = state.dp / 10,
              dv = state.dv / 10,
              dm = state.dm / 10,
              removed = state.removed + 1,
              dmIsTrailingZeros = state.dmIsTrailingZeros && (state.dm % 10 == 0),
              dvIsTrailingZeros = state.dvIsTrailingZeros && (state.lastRemovedDigit == 0),
              lastRemovedDigit = (state.dv % 10).toInt
            ),
            vplen
          )
        } else {
          val output = if (state.dvIsTrailingZeros && state.lastRemovedDigit == 5 && state.dv % 2 == 0) {
            state.dv + (if (state.dv == state.dm && !(state.dmIsTrailingZeros && c.even)) 1 else 0)
          } else {
            state.dv + (if (state.lastRemovedDigit >= 5) 1 else 0)
          }
          (output, vplen - state.removed, state.lastRemovedDigit)
        }

      for {
        vplength <- decimalLength(db.dp)
        exp = db.e10 + vplength - 1
        scientificNotation = !((exp >= -6) && (exp < 21))
        result <- {
          val (output, olength, _) = removeTrailingZeros(
            ZeroState(
              dp = db.dp,
              dv = db.dv,
              dm = db.dm,
              removed = 0,
              dmIsTrailingZeros = db.dmIsTrailingZeros,
              dvIsTrailingZeros = db.dvIsTrailingZeros,
              lastRemovedDigit = 0
            ),
            vplength
          )

          if (scientificNotation) formatScientific(output, olength, exp, ecmaMode, c.sign)
          else formatRegular(output, olength, exp, ecmaMode, c.sign)
        }
      } yield result
    }

    for {
      comp     <- components
      interval <- computeInterval(comp)
      decimal  <- computeDecimalBase(interval)
      result   <- computeOutput(decimal, interval)
    } yield result
  }

  private def formatScientific(
    output:   Long,
    olength:  Int,
    exp:      Int,
    ecmaMode: Boolean,
    sign:     Boolean
  ): F[String] = MonadThrow[F].pure {
    val result = new Array[Char](25)
    var index = 0

    if (sign) {
      result(index) = '-'
      index += 1
    }

    // Handle digits before decimal point
    var remaining = output
    for (i <- 0 until olength - 1) {
      val digit = (remaining % 10).toInt
      remaining /= 10
      result(index + olength - i) = ('0' + digit).toChar
    }
    result(index) = ('0' + remaining % 10).toChar

    // Handle decimal point
    if (olength > 1) {
      result(index + 1) = '.'
    } else if (ecmaMode) {
      // If there are no decimals, suppress .0
      index -= 1
    } else {
      result(index + 1) = '.'
      result(index + 2) = '0'
    }
    index += olength + 1

    // Add exponent
    result(index) = 'e'
    index += 1

    if (exp < 0) {
      result(index) = '-'
      index += 1
      var absExp = -exp
      if (absExp >= 100) {
        result(index) = ('0' + absExp / 100).toChar
        absExp %= 100
        result(index + 1) = ('0' + absExp / 10).toChar
        index += 2
      } else if (absExp >= 10) {
        result(index) = ('0' + absExp / 10).toChar
        index += 1
      }
      result(index) = ('0' + absExp % 10).toChar
    } else {
      result(index) = '+'
      index += 1
      if (exp >= 100) {
        result(index) = ('0' + exp / 100).toChar
        val rem = exp % 100
        result(index + 1) = ('0' + rem / 10).toChar
        index += 2
      } else if (exp >= 10) {
        result(index) = ('0' + exp / 10).toChar
        index += 1
      }
      result(index) = ('0' + exp % 10).toChar
    }
    index += 1

    new String(result, 0, index)
  }

  private def formatRegular(
    output:   Long,
    olength:  Int,
    exp:      Int,
    ecmaMode: Boolean,
    sign:     Boolean
  ): F[String] = MonadThrow[F].pure {
    val result = new Array[Char](25)
    var index = 0

    if (sign) {
      result(index) = '-'
      index += 1
    }

    var remaining = output
    if (exp < 0) {
      // Decimal point is before any digits
      result(index) = '0'
      result(index + 1) = '.'
      index += 2

      // Add leading zeros after decimal
      for (_ <- -1 until exp by -1) {
        result(index) = '0'
        index += 1
      }

      // Add significant digits
      val current = index
      for (i <- 0 until olength) {
        result(current + olength - i - 1) = ('0' + remaining % 10).toChar
        remaining /= 10
        index += 1
      }
    } else if (exp + 1 >= olength) {
      // Decimal point is after all digits
      for (i <- 0 until olength) {
        result(index + olength - i - 1) = ('0' + remaining % 10).toChar
        remaining /= 10
      }
      index += olength

      // Add trailing zeros
      for (_ <- olength until exp + 1) {
        result(index) = '0'
        index += 1
      }

      if (!ecmaMode) {
        result(index) = '.'
        result(index + 1) = '0'
        index += 2
      }
    } else {
      // Decimal point is between digits
      var current = index + 1
      for (i <- 0 until olength) {
        if (olength - i - 1 == exp) {
          result(current + olength - i - 1) = '.'
          current -= 1
        }
        result(current + olength - i - 1) = ('0' + remaining % 10).toChar
        remaining /= 10
      }
      index += olength + 1
    }

    new String(result, 0, index)
  }
}

private object DoubleSerializerImpl {
  private val DoubleMantissaBits = 52
  private val DoubleMantissaMask = (1L << DoubleMantissaBits) - 1L
  private val DoubleExponentBits = 11
  private val DoubleExponentMask = (1 << DoubleExponentBits) - 1
  private val DoubleExponentBias = (1 << (DoubleExponentBits - 1)) - 1

  private val PosTableSize = 326
  private val NegTableSize = 291
  private val Pow5BitCount = 121
  private val Pow5QuarterBitCount = 31
  private val Pow5InvBitCount = 122
  private val Pow5InvQuarterBitCount = 31

  private val (pow5Split, pow5InvSplit) = {
    val mask = (BigInt(1) << Pow5QuarterBitCount) - 1
    val invMask = (BigInt(1) << Pow5InvQuarterBitCount) - 1

    def processIndex(i: Int): (Vector[Int], Vector[Int]) = {
      val pow = BigInt(5).pow(i)
      val pow5len = pow.bitLength

      (
        calculatePowSplit(i, pow, pow5len, mask),
        calculatePowInvSplit(i, pow, pow5len, invMask)
      )
    }

    val results = (0 until math.max(PosTableSize, NegTableSize)).toVector
      .map(processIndex)

    val (powSplits, powInvSplits) = results.unzip

    val filteredPowSplits = powSplits.filter(_.nonEmpty)
    val filteredPowInvSplits = powInvSplits.filter(_.nonEmpty)

    (filteredPowSplits, filteredPowInvSplits)
  }

  private def calculatePowSplit(
    i:       Int,
    pow:     BigInt,
    pow5len: Int,
    mask:    BigInt
  ): Vector[Int] =
    if (i < PosTableSize) {
      (0 until 4).toList.map { j =>
        (pow >>
        (pow5len - Pow5BitCount + (3 - j) * Pow5QuarterBitCount) &
        mask).toInt
      }.toVector
    } else {
      Vector.empty[Int]
    }

  private def calculatePowInvSplit(
    i:       Int,
    pow:     BigInt,
    pow5len: Int,
    invMask: BigInt
  ): Vector[Int] =
    if (i < NegTableSize) {
      val j = pow5len - 1 + Pow5InvBitCount
      val inv = (BigInt(1) << j) / pow + BigInt(1)

      (0 until 4).toList.map { k =>
        if (k == 0) {
          (inv >> ((3 - k) * Pow5InvQuarterBitCount)).toInt
        } else {
          (inv >> ((3 - k) * Pow5InvQuarterBitCount) & invMask).toInt
        }
      }.toVector
    } else {
      Vector.empty[Int]
    }

  def make[F[_]: MonadThrow]: DoubleSerializerImpl[F] = new DoubleSerializerImpl[F]
}
