
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
