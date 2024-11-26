import sbt._

object Dependencies {

  object V {
    val tessellation = "2.12.0"
    val decline = "2.4.1"
    val cats = "2.9.0"
    val catsEffect = "3.4.2"
    val weaver = "0.8.1"
    val levelDb = "0.12"
    val organizeImports = "0.5.0"
    val betterMonadicFor = "0.3.1"
    val kindProjector = "0.13.3"
    val semanticDB = "4.11.2"
  }

  object Libraries {
    val tessellationSdk = "org.constellation" %% s"tessellation-sdk" % V.tessellation
    val declineCore = "com.monovore" %% "decline" % V.decline
    val cats = "org.typelevel" %% "cats-core" % V.cats
    val catsEffect = "org.typelevel" %% "cats-effect" % V.catsEffect
    val catsEffectTestkit = "org.typelevel" %% "cats-effect-testkit" % V.catsEffect
    val weaverCats = "com.disneystreaming" %% "weaver-cats" % V.weaver
    val weaverDiscipline = "com.disneystreaming" %% "weaver-discipline" % V.weaver
    val weaverScalaCheck = "com.disneystreaming" %% "weaver-scalacheck" % V.weaver
    val levelDb = "org.iq80.leveldb" % "leveldb" % V.levelDb
    val organizeImports = "com.github.liancheng" %% "organize-imports" % V.organizeImports
  }

  object CompilerPlugin {

    val betterMonadicFor = compilerPlugin(
      "com.olegpy" %% "better-monadic-for" % V.betterMonadicFor
    )

    val kindProjector = compilerPlugin(
      ("org.typelevel" % "kind-projector" % V.kindProjector).cross(CrossVersion.full)
    )

    val semanticDB = compilerPlugin(
      ("org.scalameta" % "semanticdb-scalac" % V.semanticDB).cross(CrossVersion.full)
    )
  }
}
