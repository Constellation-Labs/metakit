package crypto.mpt

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.{ExitCode, IO, IOApp}

/**
 * Writes `docs/mpt-spec/test-sealed-proofs.json` from the REAL prover output (fixture-as-golden;
 * [[MptSpecFixtureSuite]] pins the bytes and the js reference harness consumes the file). Re-run
 * only after an INTENTIONAL wire-format change, and update the reference verifiers with it:
 *
 *   sbt "Test/runMain crypto.mpt.MptSpecFixtureGenerator"
 */
object MptSpecFixtureGenerator extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    for {
      rendered <- MptSpecFixtures.rendered
      _        <- IO(Files.write(Paths.get(MptSpecFixtures.FixturePath), rendered.getBytes(StandardCharsets.UTF_8)))
      _        <- IO.println(s"Wrote ${MptSpecFixtures.FixturePath}")
    } yield ExitCode.Success
}
