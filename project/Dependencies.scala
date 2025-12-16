import sbt._

object Dependencies {

  object V {
    val tessellation = "3.5.8"
    val cats = "2.13.0"
    val catsEffect = "3.6.3"
    val weaver = "0.11.3"
    val levelDb = "0.12"
    val betterMonadicFor = "0.3.1"
    val kindProjector = "0.13.4"
    val jmh = "1.37"
  }

  object Libraries {
    val tessellationSdk = "io.constellationnetwork" %% s"tessellation-sdk" % V.tessellation
    val cats = "org.typelevel" %% "cats-core" % V.cats
    val catsEffect = "org.typelevel" %% "cats-effect" % V.catsEffect
    val catsEffectTestkit = "org.typelevel" %% "cats-effect-testkit" % V.catsEffect
    val weaverCats = "org.typelevel" %% "weaver-cats" % V.weaver
    val weaverDiscipline = "org.typelevel" %% "weaver-discipline" % V.weaver
    val weaverScalaCheck = "org.typelevel" %% "weaver-scalacheck" % V.weaver
    val levelDb = "org.iq80.leveldb" % "leveldb" % V.levelDb
  }

  object CompilerPlugin {
    val betterMonadicFor = compilerPlugin("com.olegpy" %% "better-monadic-for" % V.betterMonadicFor)
    val kindProjector = compilerPlugin(("org.typelevel" % "kind-projector" % V.kindProjector).cross(CrossVersion.full))
  }
}
