lazy val coverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false
)

lazy val buildSettings = Seq(
  organization := "com.ithaca",
  scalaVersion := "2.11.8",
  name         := "iliad",
  version      := "0.0.1-SNAPSHOT"
)

lazy val commonScalacOptions = Seq(
  "-encoding", "UTF-8",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-language:postfixOps"
)

lazy val compilerOptions = Seq(
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-target:jvm-1.7"
  ) ++ commonScalacOptions,
  javacOptions ++= Seq(
    "-source", "1.7", "-target", "1.7"
  )
)

lazy val monocleVersion = "1.2.2"
lazy val catsVersion = "0.6.1"

lazy val commonSettings = Seq(
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.jcenterRepo,
    Resolver.url(
      "bintray-zainab-ali-sbt-plugins",
      url("http://dl.bintray.com/zainab-ali/sbt-plugins"))(
      Resolver.ivyStylePatterns)
  ),
  libraryDependencies ++= Seq(
    "org.spire-math" %% "imp" % "0.2.0" % "provided",
    "org.scodec" %% "scodec-core" % "1.10.2",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "co.fs2" %% "fs2-core" % "0.9.0-M6",
    "com.chuusai" %% "shapeless" % "2.2.5",
    "org.spire-math" %% "spire" % "0.11.0",
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.typelevel" %% "cats-macros" % catsVersion,
    "org.typelevel" %% "cats-kernel" % catsVersion,
    "org.typelevel" %% "cats-free" % catsVersion,
    "org.typelevel" %% "cats-laws" % catsVersion % "test",
    "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
    "com.github.julien-truffaut"  %%  "monocle-macro" % monocleVersion,
    "com.projectseptember" %% "freek" % "0.3.0",
    "oncue.quiver" %% "core" % "5.3.57",
    "org.slf4j" % "slf4j-api" % "1.7.13",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"
  )
) ++ compilerOptions

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.spire-math" %% "spire-laws" % "0.11.0" % "test",
    "org.scalatest" %% "scalatest" % "3.0.0-M7" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
    "org.typelevel" %% "discipline" % "0.4" % "test"
  )
)

lazy val paradiseSettings = Seq(
  autoCompilerPlugins := true,
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1"),
  addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full),
  libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)
)


import sbt._
import Keys._

lazy val androidDependencies = taskKey[Seq[Attributed[File]]]("Extracts android jars and adds them to the classpath")
lazy val androidDependenciesTask = Def.task {
  val log = streams.value.log

  val androidHome = sys.env("ANDROID_HOME")
  val sdkJar = file(androidHome) / "platforms" / "android-23" / "android.jar"
  
  log.info(s"Using Android SDK jar ${sdkJar.absolutePath}")

  val supportAar = file(androidHome) / "extras" / "android" / "m2repository" / "com" / "android" / "support" / "support-v4" / "23.1.0" / "support-v4-23.1.0.aar"

    val targetDir = target.value / "android"
    IO.unzip(supportAar, targetDir)
    val supportJar = (targetDir ** "*.jar").get.head
    log.info(s"Using Android support jar ${supportJar.absolutePath}")

  Seq(supportJar, sdkJar).map(Attributed.blank)
}

lazy val androidSettings = Seq(
  androidDependencies := androidDependenciesTask.value,
  (unmanagedClasspath in Compile) := (unmanagedClasspath in Compile).value ++ 
    androidDependencies.value,
  libraryDependencies ++= Seq(
    "com.github.tony19" % "logback-android-core" % "1.1.1-5",
    "com.github.tony19" % "logback-android-classic" % "1.1.1-5" exclude("com.google.android", "android")
  )
)

lazy val desktopSettings = Seq(
  libraryDependencies ++= Seq(
    "net.java.dev.jna" % "jna-platform" % "4.2.2",
    "ch.qos.logback" % "logback-classic" % "1.1.3",
    "org.scalanlp" %% "breeze" % "0.12"
  )
)

lazy val coreSourceSettings = Seq(
  (javaSource in Directive) := (javaSource in core in Compile).value,
  (scalaSource in Directive) := (scalaSource in core in Compile).value,
  (scalaSource in Test) := (scalaSource in core in Test).value
)

lazy val macros = (project in file("macros")).settings (
  buildSettings,
  moduleName := "iliad-macros",
  commonSettings,
  testSettings,
  paradiseSettings,
  coverageSettings
)

lazy val core = (project in file("core")).settings(
  buildSettings,
  moduleName := "iliad-core",
  commonSettings,
  paradiseSettings,
  coverageSettings,	
  testSettings
).dependsOn(macros)

lazy val x11 = (project in file("modules/x11")).settings(
  buildSettings,
  moduleName := "iliad-x11",
  paradiseSettings,
  commonSettings,
  testSettings,
  desktopSettings,
  directiveSettings,
  coreSourceSettings,
  preprocessors ++= Seq(
    preprocess.skip("android"),
    preprocess.skip("win32"),
    preprocess.identity("x11"),
    preprocess.identity("desktop")
  )
).dependsOn(macros)

lazy val win32 = (project in file("modules/win32")).settings(
  buildSettings,
  moduleName := "iliad-win32",
  paradiseSettings,
  commonSettings,
  testSettings,
  desktopSettings,
  directiveSettings,
  coreSourceSettings,
  preprocessors ++= Seq(
    preprocess.skip("android"),
    preprocess.skip("x11"),
    preprocess.identity("win32"),
    preprocess.identity("desktop")
  )
).dependsOn(macros)


lazy val android = (project in file("modules/android")).settings(
  buildSettings,
  moduleName := "iliad-android",
  paradiseSettings,
  androidSettings,
  commonSettings,
  testSettings,
  directiveSettings,
  coreSourceSettings,
  preprocessors ++= Seq(
    preprocess.skip("x11"),
    preprocess.skip("win32"),
    preprocess.skip("desktop"),
    preprocess.identity("android")
  )
).dependsOn(macros)


lazy val root = (project in file(".")).settings(
  buildSettings,
  paradiseSettings,
  compilerOptions,
  moduleName := "iliad"
).aggregate(macros, win32, x11)
