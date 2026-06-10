package crypto.smt

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import cats.effect.{IO, Resource}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.smt._
import io.constellationnetwork.metagraph_sdk.crypto.smt.api.SparseMerkleVerifier
import io.constellationnetwork.metagraph_sdk.crypto.smt.impl.{InMemorySparseMerkleTree, LevelDbSparseMerkleTree}
import io.constellationnetwork.security.hex.Hex

import weaver._

/**
 * Persistence + parity tests for [[LevelDbSparseMerkleTree]] -- the LevelDB-backed sibling of
 * [[InMemorySparseMerkleTree]]. Mirrors the MPT `LevelDbMerklePatriciaProducerSuite` temp-dir / restart idiom:
 *   - the LevelDB-built root MATCHES an in-memory tree over the same entries (the persistent variant computes
 *     byte-identical digests),
 *   - state SURVIVES a Resource release/reopen (root unchanged, membership proof still verifies),
 *   - [[LevelDbSparseMerkleTree.remove]] advances the root, and
 *   - [[LevelDbSparseMerkleTree.get]] returns the EXACT value bytes (base64 round-trip is lossless).
 */
object LevelDbSparseMerkleTreeSuite extends SimpleIOSuite {

  /** N distinct 32-byte keys with small distinct values. */
  private def testData(n: Int): Map[Hex, Array[Byte]] =
    (0 until n).map { i =>
      val key = Hex.fromBytes(Array.tabulate[Byte](32)(j => (i * 31 + j).toByte))
      val value = s"value-$i".getBytes("UTF-8")
      key -> value
    }.toMap

  private def tempDbPath: Resource[IO, Path] =
    Resource.make(IO(Files.createTempDirectory("smt-leveldb-test")))(deleteRecursively)

  private def deleteRecursively(path: Path): IO[Unit] = IO {
    Files.walkFileTree(
      path,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      }
    )
  } *> IO.unit

  test("(1) LevelDB-built root equals in-memory tree built from the same entries") {
    tempDbPath.use { dbPath =>
      val data = testData(8)
      for {
        levelDbRoot <- LevelDbSparseMerkleTree.make[IO](dbPath, data).use(_.root)
        inMemRoot   <- InMemorySparseMerkleTree.make[IO](data).flatMap(_.root)
      } yield expect(levelDbRoot === inMemRoot)
    }
  }

  test("(2) state persists across Resource release: reload keeps the root and a membership proof verifies") {
    tempDbPath.use { dbPath =>
      val data = testData(6)
      val (probeKey, probeValue) = (data.head._1, data.head._2)
      val verifier = SparseMerkleVerifier.make[IO]

      for {
        // First instance: seed empty, insert all, capture the root.
        rootBefore <- LevelDbSparseMerkleTree.make[IO](dbPath).use { tree =>
          data.toList.traverse_ { case (k, v) => tree.insert(k, v) } >> tree.root
        }

        // Second instance: reload from disk (must NOT be empty).
        reloaded <- LevelDbSparseMerkleTree.load[IO](dbPath).use { tree =>
          for {
            rootAfter <- tree.root
            prover    <- tree.prover
            proofE    <- prover.prove(probeKey)
            verifiedOk <- proofE match {
              case Right(incl @ SparseMerkleProof.Inclusion(_, _, _, _)) =>
                verifier.verify(rootAfter, incl).map {
                  case Right(v) =>
                    v.value match {
                      case SparseMerkleEntry.Present(k, value) => expect(k === probeKey) && expect(value.sameElements(probeValue))
                      case other                               => failure(s"expected Present, got $other")
                    }
                  case Left(err) => failure(s"verify rejected a valid inclusion after reload: $err")
                }
              case Right(other) => IO.pure(failure(s"expected Inclusion for present key, got $other"))
              case Left(err)    => IO.pure(failure(s"prove failed after reload: $err"))
            }
          } yield (rootAfter, verifiedOk)
        }
      } yield expect(reloaded._1 === rootBefore) && reloaded._2
    }
  }

  test("(2b) load fails on an empty / absent database") {
    tempDbPath.use { dbPath =>
      LevelDbSparseMerkleTree.load[IO](dbPath).use(_ => IO.unit).attempt.map(r => expect(r.isLeft))
    }
  }

  test("(3) remove updates the root") {
    tempDbPath.use { dbPath =>
      val data = testData(5)
      val victim = data.head._1
      LevelDbSparseMerkleTree.make[IO](dbPath, data).use { tree =>
        for {
          rootBefore <- tree.root
          rootAfter  <- tree.remove(victim)
          gone       <- tree.get(victim)
          reference  <- InMemorySparseMerkleTree.make[IO](data - victim).flatMap(_.root)
        } yield expect(rootBefore =!= rootAfter) && expect(gone.isEmpty) && expect(rootAfter === reference)
      }
    }
  }

  test("(4) get returns the exact value bytes") {
    tempDbPath.use { dbPath =>
      // Include a value with the full byte range to stress the base64 round-trip.
      val key = Hex.fromBytes(Array.tabulate[Byte](32)(i => (i + 1).toByte))
      val value = Array.tabulate[Byte](256)(i => i.toByte) // 0x00..0xff, all byte values
      for {
        // First instance: write and read back (release before reloading -- LevelDB holds an exclusive file lock).
        fetched <- LevelDbSparseMerkleTree.make[IO](dbPath, Map(key -> value)).use(_.get(key))
        // Second instance: the bytes survive a release/reopen round-trip on disk.
        fromDisk <- LevelDbSparseMerkleTree.load[IO](dbPath).use(_.get(key))
      } yield expect(fetched.exists(_.sameElements(value))) && expect(fromDisk.exists(_.sameElements(value)))
    }
  }

  test("(5) withChanges applies removes-then-upserts and matches an in-memory tree") {
    tempDbPath.use { dbPath =>
      val data = testData(6)
      val (toRemove, toKeep) = data.toList.splitAt(3)
      val removeKeys = toRemove.map(_._1).toSet
      val extra = Hex.fromBytes(Array.fill[Byte](32)(0x5a.toByte)) -> "extra".getBytes("UTF-8")
      val upserts = (toKeep :+ extra).toMap

      LevelDbSparseMerkleTree.make[IO](dbPath, data).use { tree =>
        for {
          newRoot   <- tree.withChanges(upserts, removeKeys)
          entries   <- tree.entries
          reference <- InMemorySparseMerkleTree.make[IO]((data -- removeKeys) ++ upserts).flatMap(_.root)
        } yield
          expect(newRoot === reference) &&
          expect(removeKeys.forall(k => !entries.contains(k))) &&
          expect(entries.contains(extra._1))
      }
    }
  }
}
