
import Dependencies.*
import sbt.*
import sbt.Keys.*

ThisBuild / scalaVersion := "2.13.16"
ThisBuild / organization := "io.constellationnetwork"
ThisBuild / homepage := Some(url("https://github.com/Constellation-Labs/metakit"))
ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / sonatypeCredentialHost := "central.sonatype.com"
ThisBuild / versionScheme := Some("early-semver")

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

ThisBuild / developers := List(
  Developer(
    "constellation-contributors",
    "Constellation Contributors",
    "contact@constellationnetwork.io",
    url("https://github.com/Constellation-Labs/metakit/graphs/contributors")
  )
)

ThisBuild / evictionErrorLevel := Level.Warn

lazy val commonSettings = Seq(
  scalacOptions ++= List("-Ymacro-annotations", "-Yrangepos", "-Wconf:cat=unused:info", "-language:reflectiveCalls"),
  scalafmtOnCompile := true,
  scalafixOnCompile := true,
  resolvers += Resolver.mavenLocal,
  libraryDependencies ++= Seq(
    CompilerPlugin.kindProjector,
    CompilerPlugin.betterMonadicFor,
    Libraries.tessellationSdk,
    Libraries.cats,
    Libraries.catsEffect,
    Libraries.levelDb
  ),
  // BLS-VENDORED-BETA: vendored BouncyCastle 1.85 beta until 1.85 is a stable managed dep.
  // Mirrors the tessellation-bls build.sbt hack EXACTLY (canonical reference). The BLS12-381
  // API (org.bouncycastle.crypto.bls.*) used by our eth2-ciphersuite BLS primitive
  // (io.constellationnetwork.metagraph_sdk.crypto.bls.Bls12381) exists ONLY in the 1.85 beta
  // jars dropped in ./lib (auto-picked up via sbt's unmanagedBase = <project>/lib). The
  // tessellation-sdk dependency pulls BouncyCastle 1.70 (bcprov/bcpkix/bcutil-jdk15on)
  // TRANSITIVELY; we drop that here so only the vendored 1.85 jdk18on jars provide
  // org.bouncycastle on the classpath -- otherwise the 1.70 classes shadow the 1.85 ones and
  // the org.bouncycastle.crypto.bls package is missing at compile time.
  // MIGRATION DELTA when 1.85 is published: delete ./lib/*.jar, drop this excludeDependencies
  // block, and add a managed `org.bouncycastle %% bcprov-jdk18on % 1.85` dependency.
  excludeDependencies ++= Seq(
    ExclusionRule("org.bouncycastle", "bcprov-jdk15on"),
    ExclusionRule("org.bouncycastle", "bcpkix-jdk15on"),
    ExclusionRule("org.bouncycastle", "bcutil-jdk15on")
  )
) ++ Defaults.itSettings

lazy val commonTestSettings = Seq(
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  scalafmtOnCompile := true,
  scalafixOnCompile := true,
  libraryDependencies ++= Seq(
    Libraries.weaverCats,
    Libraries.weaverDiscipline,
    Libraries.weaverScalaCheck,
    Libraries.catsEffectTestkit
  ).map(_ % Test)
)

lazy val buildInfoSettings = Seq(
  buildInfoKeys := Seq[BuildInfoKey](
    name,
    version,
    scalaVersion,
    sbtVersion
  ),
  buildInfoPackage := "io.constellationnetwork.buildinfo"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    commonTestSettings,
    name := "metakit"
  )

lazy val benchmarks = (project in file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .dependsOn(root)
  .settings(
    commonSettings,
    name := "metakit-benchmarks",
    publish / skip := true
  )
