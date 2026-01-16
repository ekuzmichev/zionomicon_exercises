ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "ru.ekuzmichev"
ThisBuild / scalaVersion := "3.3.7"

val zioVersion = "2.1.24"

lazy val root = (project in file("."))
  .settings(
    name := "zionomicon_exercises",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"      % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion
    )
  )
