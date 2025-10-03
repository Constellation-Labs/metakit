package io.constellationnetwork.metagraph_sdk.storage.impl

import java.nio.file.{Files, Path}

import cats.data.EitherT
import cats.effect.syntax.spawn._
import cats.effect.{Async, Fiber, Resource, Sync}
import cats.syntax.all._

import scala.concurrent.duration._

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec._
import io.constellationnetwork.metagraph_sdk.storage.Collection

import io.circe.{Decoder, Encoder}
import org.iq80.leveldb._
import org.iq80.leveldb.impl.Iq80DBFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object LevelDbCollection {

  def make[F[_]: Async, Key: Encoder: Decoder, Value: Encoder: Decoder](
    path: Path,
    compactionInterval: FiniteDuration = 1.hour
  ): Resource[F, Collection[F, Key, Value]] = for {
    implicit0(logger: Logger[F]) <- Resource.pure(Slf4jLogger.getLoggerFromClass(this.getClass))
    ldbFactory                   <- Resource.eval(Sync[F].delay(Iq80DBFactory.factory: DBFactory))
    db                           <- LevelDbCollection.makeDb[F](path, ldbFactory)
    _                            <- startBackgroundCompaction[F](db, path, compactionInterval)
  } yield
    new Collection[F, Key, Value] {

      def put(id: Key, t: Value): F[Unit] = for {
        idB <- id.toBinary
        tB  <- t.toBinary
        _   <- Sync[F].blocking(db.put(idB, tB))
      } yield ()

      def remove(id: Key): F[Unit] =
        id.toBinary.flatMap { idB =>
          Sync[F].blocking(db.delete(idB))
        }

      def get(id: Key): F[Option[Value]] =
        id.toBinary
          .flatMap(idB => Sync[F].blocking(Option(db.get(idB))))
          .flatMap(_.flatTraverse(_.fromBinary[Value].map(_.toOption)))

      def contains(id: Key): F[Boolean] =
        id.toBinary.flatMap { idB =>
          Sync[F].blocking(db.get(idB)).map(_ != null)
        }

      def putBatch(updates: List[(Key, Value)]): F[Unit] =
        createWriteResource.use {
          case (batch, wo) =>
            for {
              _ <- updates.traverse_ {
                case (id, t) =>
                  (id.toBinary, t.toBinary).tupled.map { case (idB, tB) => batch.put(idB, tB) }
              }
              _ <- Sync[F].blocking(db.write(batch, wo.sync(true)))
            } yield ()
        }

      def removeBatch(deletions: List[Key]): F[Unit] =
        createWriteResource.use {
          case (batch, wo) =>
            for {
              _ <- deletions.traverse_ { id =>
                id.toBinary.map(idB => batch.delete(idB))
              }
              _ <- Sync[F].blocking(db.write(batch, wo.sync(true)))
            } yield ()
        }

      def getBatch(keys: List[Key]): F[List[(Key, Option[Value])]] =
        createReadResource.use { readOptions =>
          keys.traverse { id =>
            id.toBinary
              .flatMap(idB => Sync[F].blocking(Option(db.get(idB, readOptions))))
              .flatMap(_.flatTraverse(_.fromBinary[Value].map(_.toOption)))
              .map((id, _))
          }
        }

      def getWithFilter(
        cond: (Key, Value) => Boolean
      ): F[List[(Key, Value)]] = loopWithConditionAndLimit(cond)

      private def loopWithConditionAndLimit(
        cond: (Key, Value) => Boolean,
        limit: Int = Int.MaxValue
      ): F[List[(Key, Value)]] = {
        def loop(
          iter: DBIterator,
          buf: List[(Key, Value)],
          count: Int = 0
        ): F[List[(Key, Value)]] =
          Sync[F].tailRecM((iter, buf, count)) {
            case (_iter, _buf, _count) =>
              if (!_iter.hasNext || _count >= limit) Sync[F].pure(Right(_buf))
              else {
                (for {
                  entry <- EitherT(Sync[F].delay(_iter.next()).attempt)
                  key   <- EitherT[F, Throwable, Key](entry.getKey.fromBinary[Key])
                  value <- EitherT[F, Throwable, Value](entry.getValue.fromBinary[Value])
                } yield (key, value)).value.flatMap {
                  case Left(_) => Sync[F].pure(Right(_buf))
                  case Right((key, value)) =>
                    val newBuffer = if (cond(key, value)) _buf :+ (key, value) else _buf
                    Sync[F].pure(Left((_iter, newBuffer, _count + 1)))
                }
              }
          }

        createIterResource.use {
          case (iter, _) =>
            loop(iter, List.empty[(Key, Value)])
        }
      }

      private def createReadResource: Resource[F, ReadOptions] =
        Resource.make {
          for {
            snapshot <- Sync[F].delay(db.getSnapshot)
            ro = new ReadOptions().snapshot(snapshot)
          } yield ro
        } { ro =>
          Sync[F].delay(ro.snapshot().close())
        }

      private def createWriteResource: Resource[F, (WriteBatch, WriteOptions)] =
        Resource.make {
          Sync[F].delay((db.createWriteBatch(), new WriteOptions()))
        } {
          case (batch, _) =>
            Sync[F].delay(batch.close())
        }

      private def createIterResource: Resource[F, (DBIterator, ReadOptions)] =
        Resource.make {
          for {
            snapshot <- Sync[F].delay(db.getSnapshot)
            ro = new ReadOptions().snapshot(snapshot)
            iter <- Sync[F].delay(db.iterator(ro))
            _    <- Sync[F].delay(iter.seekToFirst())
          } yield (iter, ro)
        } {
          case (iter, ro) =>
            for {
              _ <- Sync[F].delay(iter.close())
              _ <- Sync[F].delay(ro.snapshot().close())
            } yield ()
        }
    }

  def makeDb[F[_]: Sync](
    baseDirectory: Path,
    factory: DBFactory,
    createIfMissing: Boolean = true,
    paranoidChecks: Option[Boolean] = None,
    blockSize: Option[Int] = None,
    cacheSize: Option[Long] = None,
    maxOpenFiles: Option[Int] = None,
    compressionType: Option[CompressionType] = None
  ): Resource[F, DB] = {
    val options = new Options
    options.createIfMissing(createIfMissing)
    paranoidChecks.foreach(options.paranoidChecks)
    blockSize.foreach(options.blockSize)
    cacheSize.foreach(options.cacheSize)
    maxOpenFiles.foreach(options.maxOpenFiles)
    compressionType.foreach(options.compressionType)

    val dbF =
      Sync[F].whenA(createIfMissing)(Sync[F].blocking(Files.createDirectories(baseDirectory))) >>
      Sync[F].blocking {
        factory.open(
          baseDirectory.toFile,
          options
        )
      }

    Resource.fromAutoCloseable(dbF)
  }

  private def startBackgroundCompaction[F[_]: Async](
    db: DB,
    path: Path,
    interval: FiniteDuration
  )(implicit logger: Logger[F]): Resource[F, Fiber[F, Throwable, Nothing]] = {
    val compactionLoop =
      for {
        _ <- Async[F].sleep(interval)
        _ <- Sync[F].blocking(db.compactRange(null, null)).attempt.flatMap {
          case Left(error) =>
            logger.error(s"LevelDB compaction failed for $path: ${error.getMessage}")
          case Right(_) =>
            logger.debug(s"LevelDB compaction completed for $path")
        }
      } yield ()

    Resource.make(
      compactionLoop.foreverM.start
    )(_.cancel)
  }
}
