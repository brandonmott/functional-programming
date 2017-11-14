import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.brandonmott",
      scalaVersion := "2.12.3",
      version      := "0.0.1"
    )),
    name := "functional",
    libraryDependencies ++= Seq(
      "org.scalatest"   %% "scalatest"  % scalaTest % Test,
      "org.scalacheck"  %% "scalacheck" % scalaCheck % "test",
      "org.typelevel"   %% "cats-core"  % "1.0.0-MF"
    )
  )
