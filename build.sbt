import Dependencies.*
import sbt.*
import sbt.Keys.*

ThisBuild / scalaVersion := "2.13.15"
ThisBuild / organization := "io.constellationnetwork"
ThisBuild / homepage := Some(url("https://github.com/Constellation-Labs/metakit"))
ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / sonatypeCredentialHost := "central.sonatype.com"
ThisBuild / versionScheme := Some("early-semver")

ThisBuild / developers := List(
  Developer(
    "constellation-contributors",
    "Constellation Contributors",
    "contact@constellationnetwork.io",
    url("https://github.com/Constellation-Labs/metakit/graphs/contributors")
  )
)

ThisBuild / evictionErrorLevel := Level.Warn
ThisBuild / scalafixDependencies += Libraries.organizeImports

lazy val commonSettings = Seq(
  scalacOptions ++= List("-Ymacro-annotations", "-Yrangepos", "-Wconf:cat=unused:info", "-language:reflectiveCalls"),
  resolvers += Resolver.mavenLocal,
  libraryDependencies ++= Seq(
    CompilerPlugin.kindProjector,
    CompilerPlugin.betterMonadicFor,
    CompilerPlugin.semanticDB,
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
  .enablePlugins(JmhPlugin)
  .settings(
    commonSettings,
    commonTestSettings,
    name := "metakit"
  )
