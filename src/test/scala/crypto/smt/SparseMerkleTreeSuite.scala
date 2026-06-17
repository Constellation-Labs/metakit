package crypto.smt

import cats.Show
import cats.effect.IO
import cats.syntax.all._

import scala.util.Random

import io.constellationnetwork.metagraph_sdk.crypto.smt._
import io.constellationnetwork.metagraph_sdk.crypto.smt.api.SparseMerkleVerifier
import io.constellationnetwork.metagraph_sdk.crypto.smt.impl.InMemorySparseMerkleTree
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * Property + unit tests for the additive Sparse Merkle Tree primitive ported from tessellation-nakamoto.
 *
 * Proves the two headline properties of this primitive:
 *   - ABSENCE is native and first-class: prove an absent key, verify the absence, then show inserting it changes the
 *     root.
 *   - ORDER-INDEPENDENCE: the same key-set inserted/removed in any permutation yields the same root.
 *
 * Plus the verifier's contract bar: mandatory value-binding ([[SparseMerkleProofError.ValueBindingFailed]]) and
 * authentication-path soundness ([[SparseMerkleProofError.RootMismatch]] / [[SparseMerkleProofError.MalformedProof]]).
 *
 * Uses `SimpleIOSuite` with `JsonBinaryHasher[IO]` derived implicitly -- exactly the metakit crypto/mpt suite idiom
 * (no `Hasher`/`SecurityProvider` resource is needed, since hashing routes through `std/JsonBinaryHasher`).
 */
object SparseMerkleTreeSuite extends SimpleIOSuite with Checkers {

  // ---- generators -------------------------------------------------------------------------------------------------------------------------

  /** A 32-byte key as a 64-char hex string (any Hex works since the position is `hash(key)`; fixed-width keeps samples readable). */
  private val keyGen: Gen[Hex] =
    Gen.listOfN(32, Gen.chooseNum(0, 255)).map(bs => Hex.fromBytes(bs.map(_.toByte).toArray))

  /** A small non-empty value. */
  private val valueGen: Gen[Array[Byte]] =
    Gen.choose(1, 24).flatMap(n => Gen.listOfN(n, Gen.chooseNum(0, 255)).map(_.map(_.toByte).toArray))

  /** A set of entries with DISTINCT keys (1..16). */
  final case class Entries(pairs: List[(Hex, Array[Byte])])

  implicit val showEntries: Show[Entries] = Show.show(e => s"Entries(n=${e.pairs.size}, keys=${e.pairs.map(_._1.shortValue)})")

  /** Entries with between `minSize` and 16 distinct keys. Over-samples keys then de-dups + takes, keeping the constraint INSIDE the generator. */
  private def entriesGenMin(minSize: Int): Gen[Entries] = for {
    n          <- Gen.chooseNum(minSize, 16)
    candidates <- Gen.listOfN(n + 8, keyGen).map(_.distinctBy(_.value))
    keys = candidates.take(n)
    values <- Gen.listOfN(keys.size, valueGen)
  } yield Entries(keys.zip(values))

  private val entriesGen: Gen[Entries] = entriesGenMin(1)

  /** Entries plus one extra key guaranteed NOT in the set (for absence tests). */
  final case class EntriesWithAbsent(entries: Entries, absent: Hex)

  implicit val showEntriesWithAbsent: Show[EntriesWithAbsent] =
    Show.show(e => s"EntriesWithAbsent(${showEntries.show(e.entries)}, absent=${e.absent.shortValue})")

  private val entriesWithAbsentGen: Gen[EntriesWithAbsent] = for {
    entries <- entriesGen
    absent  <- keyGen.suchThat(k => !entries.pairs.exists(_._1.value == k.value))
  } yield EntriesWithAbsent(entries, absent)

  // ---- helpers ----------------------------------------------------------------------------------------------------------------------------

  private def buildInOrder(pairs: List[(Hex, Array[Byte])]): IO[SparseMerkleRoot] =
    InMemorySparseMerkleTree.make[IO](pairs.toMap).flatMap(_.root)

  // ---- (a) ORDER-INDEPENDENCE -------------------------------------------------------------------------------------------------------------

  test("(a) order-independence: same key-set inserted in any permutation yields the same root") {
    forall(entriesGen) { entries =>
      val canonical = entries.pairs
      val perm1 = new Random(1).shuffle(entries.pairs)
      val perm2 = new Random(2).shuffle(entries.pairs)
      val perm3 = entries.pairs.reverse

      for {
        r0 <- buildInOrder(canonical)
        r1 <- buildInOrder(perm1)
        r2 <- buildInOrder(perm2)
        r3 <- buildInOrder(perm3)
      } yield expect.all(r0 === r1, r0 === r2, r0 === r3)
    }
  }

  test("(a') order-independence under interleaved remove: insert-all then remove-half in two orders agrees") {
    forall(entriesGenMin(2)) { entries =>
      val (toRemove, toKeep) = entries.pairs.splitAt(entries.pairs.size / 2)
      val removeKeys = toRemove.map(_._1).toSet

      for {
        full         <- InMemorySparseMerkleTree.make[IO](entries.pairs.toMap)
        afterRemove1 <- full.withChanges(Map.empty, removeKeys)
        r1           <- afterRemove1.root

        empty        <- InMemorySparseMerkleTree.empty[IO]
        afterRemove2 <- empty.withChanges(entries.pairs.toMap, Set.empty).flatMap(_.withChanges(Map.empty, removeKeys))
        r2           <- afterRemove2.root

        ref <- buildInOrder(toKeep)
      } yield expect.all(r1 === r2, r1 === ref)
    }
  }

  test("(a'') empty tree has the default-placeholder root") {
    for {
      tree <- InMemorySparseMerkleTree.empty[IO]
      r    <- tree.root
    } yield expect(r === SparseMerkleRoot.empty)
  }

  // ---- (b) INCLUSION round-trip -----------------------------------------------------------------------------------------------------------

  test("(b) inclusion round-trip: prove(present) verifies and returns the bound value") {
    val verifier = SparseMerkleVerifier.make[IO]

    forall(entriesGen) { entries =>
      InMemorySparseMerkleTree.make[IO](entries.pairs.toMap).flatMap { tree =>
        tree.root.flatMap { root =>
          tree.prover.flatMap { prover =>
            entries.pairs.traverse {
              case (key, value) =>
                prover.prove(key).flatMap {
                  case Right(proof @ SparseMerkleProof.Inclusion(_, _, _, _)) =>
                    verifier.verify(root, proof).map {
                      case Right(verified) =>
                        verified.value match {
                          case SparseMerkleEntry.Present(k, v) => expect(k === key) && expect(v.toBytes.sameElements(value))
                          case other                           => failure(s"expected Present, got $other")
                        }
                      case Left(err) => failure(s"verify rejected a valid inclusion: $err")
                    }
                  case Right(other) => IO.pure(failure(s"expected Inclusion for present key, got $other"))
                  case Left(err)    => IO.pure(failure(s"prove failed for present key: $err"))
                }
            }.map(_.combineAll)
          }
        }
      }
    }
  }

  // ---- (c) ABSENCE round-trip -------------------------------------------------------------------------------------------------------------

  test("(c) absence round-trip: prove(absent) verifies as Absent, then inserting the key CHANGES the root") {
    val verifier = SparseMerkleVerifier.make[IO]

    forall(entriesWithAbsentGen) { ewa =>
      InMemorySparseMerkleTree.make[IO](ewa.entries.pairs.toMap).flatMap { tree =>
        for {
          rootBefore <- tree.root
          prover     <- tree.prover
          proofE     <- prover.prove(ewa.absent)
          absenceOk <- proofE match {
            case Right(proof @ SparseMerkleProof.Absence(k, _, _)) =>
              verifier.verify(rootBefore, proof).map {
                case Right(verified) =>
                  verified.value match {
                    case SparseMerkleEntry.Absent(ak) => expect(ak === k) && expect(k === ewa.absent)
                    case other                        => failure(s"expected Absent, got $other")
                  }
                case Left(err) => failure(s"verify rejected a valid absence: $err")
              }
            case Right(other) => IO.pure(failure(s"expected Absence for absent key, got $other"))
            case Left(err)    => IO.pure(failure(s"prove failed for absent key: $err"))
          }
          afterInsert <- tree.insert(ewa.absent, "now-present".getBytes("UTF-8"))
          rootAfter   <- afterInsert.root
        } yield absenceOk && expect(rootBefore =!= rootAfter)
      }
    }
  }

  test("(c') absence on the EMPTY tree verifies as Default-witness Absent against the empty root") {
    val verifier = SparseMerkleVerifier.make[IO]
    val key = Hex.fromBytes(Array.fill[Byte](32)(7))

    for {
      tree   <- InMemorySparseMerkleTree.empty[IO]
      root   <- tree.root
      prover <- tree.prover
      proofE <- prover.prove(key)
      result <- proofE match {
        case Right(proof @ SparseMerkleProof.Absence(_, AbsenceWitness.Default, siblings)) =>
          verifier.verify(root, proof).map { v =>
            expect(siblings.isEmpty) && expect(v.isRight)
          }
        case other => IO.pure(failure(s"expected Default-witness Absence with no siblings, got $other"))
      }
    } yield result
  }

  test("(c'') single-leaf tree: absence of a different key is an OtherLeaf witness that verifies") {
    val verifier = SparseMerkleVerifier.make[IO]
    val presentKey = Hex.fromBytes(Array.fill[Byte](32)(1))
    val absentKey = Hex.fromBytes(Array.fill[Byte](32)(2))

    for {
      tree   <- InMemorySparseMerkleTree.make[IO](Map(presentKey -> "v".getBytes("UTF-8")))
      root   <- tree.root
      prover <- tree.prover
      proofE <- prover.prove(absentKey)
      result <- proofE match {
        case Right(proof @ SparseMerkleProof.Absence(_, AbsenceWitness.OtherLeaf(occKey, _), _)) =>
          verifier.verify(root, proof).map { v =>
            expect(occKey === presentKey) && expect(v.isRight)
          }
        case other => IO.pure(failure(s"expected OtherLeaf Absence, got $other"))
      }
    } yield result
  }

  // ---- (d) VALUE-TAMPER => ValueBindingFailed ----------------------------------------------------------------------------------------------

  test("(d) value-tamper: mutating the inclusion proof's value bytes => ValueBindingFailed") {
    val verifier = SparseMerkleVerifier.make[IO]

    forall(entriesGen) { entries =>
      InMemorySparseMerkleTree.make[IO](entries.pairs.toMap).flatMap { tree =>
        tree.root.flatMap { root =>
          tree.prover.flatMap { prover =>
            val (key, _) = entries.pairs.head
            prover.prove(key).flatMap {
              case Right(SparseMerkleProof.Inclusion(k, value, vd, siblings)) =>
                val tampered =
                  SparseMerkleProof.Inclusion(k, Hex.fromBytes(value.toBytes :+ 0x7f.toByte), vd, siblings) // value no longer hashes to vd
                verifier.verify(root, tampered).map {
                  case Left(SparseMerkleProofError.ValueBindingFailed(bad)) => expect(bad === k)
                  case other                                                => failure(s"expected ValueBindingFailed, got $other")
                }
              case other => IO.pure(failure(s"expected Inclusion, got $other"))
            }
          }
        }
      }
    }
  }

  // ---- (e) tampered authentication path => rejection ---------------------------------------------------------------------------------------

  test("(e1) wrong sibling: corrupting a sibling digest => RootMismatch") {
    val verifier = SparseMerkleVerifier.make[IO]

    forall(entriesGenMin(4)) { entries =>
      InMemorySparseMerkleTree.make[IO](entries.pairs.toMap).flatMap { tree =>
        tree.root.flatMap { root =>
          tree.prover.flatMap { prover =>
            entries.pairs.collectFirstSomeM {
              case (key, _) =>
                prover.prove(key).map {
                  case Right(SparseMerkleProof.Inclusion(k, v, vd, siblings)) if siblings.nonEmpty =>
                    val corrupted = siblings.updated(0, SparseMerkleSibling(Hash("f" * 64)))
                    Some(SparseMerkleProof.Inclusion(k, v, vd, corrupted))
                  case _ => None
                }
            }.flatMap {
              case Some(badProof) =>
                verifier.verify(root, badProof).map {
                  case Left(SparseMerkleProofError.RootMismatch(_, _)) => success
                  case other                                           => failure(s"expected RootMismatch, got $other")
                }
              case None => IO.pure(success) // no multi-sibling proof in this sample; nothing to corrupt
            }
          }
        }
      }
    }
  }

  test("(e2) omitted sibling: dropping a sibling from a deep proof => RootMismatch") {
    val verifier = SparseMerkleVerifier.make[IO]

    forall(entriesGenMin(4)) { entries =>
      InMemorySparseMerkleTree.make[IO](entries.pairs.toMap).flatMap { tree =>
        tree.root.flatMap { root =>
          tree.prover.flatMap { prover =>
            entries.pairs.collectFirstSomeM {
              case (key, _) =>
                prover.prove(key).map {
                  case Right(SparseMerkleProof.Inclusion(k, v, vd, siblings)) if siblings.nonEmpty =>
                    Some(SparseMerkleProof.Inclusion(k, v, vd, siblings.drop(1)))
                  case _ => None
                }
            }.flatMap {
              case Some(badProof) =>
                verifier.verify(root, badProof).map {
                  case Left(SparseMerkleProofError.RootMismatch(_, _)) => success
                  case other => failure(s"expected RootMismatch after omitted sibling, got $other")
                }
              case None => IO.pure(success)
            }
          }
        }
      }
    }
  }

  test("(e3) extra sibling: appending a bogus sibling => RootMismatch") {
    val verifier = SparseMerkleVerifier.make[IO]

    forall(entriesGen) { entries =>
      InMemorySparseMerkleTree.make[IO](entries.pairs.toMap).flatMap { tree =>
        tree.root.flatMap { root =>
          tree.prover.flatMap { prover =>
            val (key, _) = entries.pairs.head
            prover.prove(key).flatMap {
              case Right(SparseMerkleProof.Inclusion(k, v, vd, siblings)) =>
                val withExtra = SparseMerkleProof.Inclusion(k, v, vd, siblings :+ SparseMerkleSibling(Hash.empty))
                verifier.verify(root, withExtra).map {
                  case Left(SparseMerkleProofError.RootMismatch(_, _)) => success
                  case other                                           => failure(s"expected RootMismatch after extra sibling, got $other")
                }
              case other => IO.pure(failure(s"expected Inclusion, got $other"))
            }
          }
        }
      }
    }
  }

  test("(e4) wrong-key inclusion: an inclusion proof verified against a DIFFERENT root => rejection") {
    val verifier = SparseMerkleVerifier.make[IO]
    val keyA = Hex.fromBytes(Array.fill[Byte](32)(10))
    val keyB = Hex.fromBytes(Array.fill[Byte](32)(20))

    for {
      treeA   <- InMemorySparseMerkleTree.make[IO](Map(keyA -> "a".getBytes("UTF-8")))
      treeB   <- InMemorySparseMerkleTree.make[IO](Map(keyB -> "b".getBytes("UTF-8")))
      rootB   <- treeB.root
      proverA <- treeA.prover
      proofA  <- proverA.prove(keyA)
      result <- proofA match {
        case Right(incl: SparseMerkleProof.Inclusion) =>
          verifier.verify(rootB, incl).map {
            case Left(SparseMerkleProofError.RootMismatch(_, _)) => success
            case other                                           => failure(s"expected RootMismatch against foreign root, got $other")
          }
        case other => IO.pure(failure(s"expected Inclusion, got $other"))
      }
    } yield result
  }

  // ---- proof JSON round-trips byte-exactly --------------------------------------------------------------------------------------------------

  test("proof Circe round-trip preserves inclusion + absence proofs byte-exactly") {
    import io.circe.syntax._

    forall(entriesWithAbsentGen) { ewa =>
      InMemorySparseMerkleTree.make[IO](ewa.entries.pairs.toMap).flatMap { tree =>
        tree.prover.flatMap { prover =>
          val (presentKey, _) = ewa.entries.pairs.head
          for {
            inclE <- prover.prove(presentKey)
            absE  <- prover.prove(ewa.absent)
          } yield {
            val incl = inclE.toOption.get
            val abs = absE.toOption.get
            val inclRound = incl.asJson.as[SparseMerkleProof]
            val absRound = abs.asJson.as[SparseMerkleProof]
            expect(inclRound === Right(incl)) && expect(absRound === Right(abs))
          }
        }
      }
    }
  }

  // ---- collapse invariant sanity (white-box): node count is bounded, not 2^256 ---------------------------------------------------------

  test("structural: a 2-leaf tree materializes a bounded stem (no 2^256 blow-up)") {
    val k1 = Hex.fromBytes(Array.fill[Byte](32)(0))
    val k2 = Hex.fromBytes(Array.fill[Byte](32)(0xff.toByte))

    for {
      tree   <- InMemorySparseMerkleTree.make[IO](Map(k1 -> "a".getBytes("UTF-8"), k2 -> "b".getBytes("UTF-8")))
      prover <- tree.prover
      p1     <- prover.prove(k1)
      p2     <- prover.prove(k2)
    } yield {
      val d1 = p1.toOption.get.siblings.size
      val d2 = p2.toOption.get.siblings.size
      expect(d1 > 0) && expect(d1 <= SparseMerkleHashing.PositionBits) && expect(d2 <= SparseMerkleHashing.PositionBits) && expect(
        d1 === d2
      )
    }
  }
}
