name := "tasty-inspection"

version := "0.1"

val scala3Version = "3.1.0"

lazy val inspector = (project in file("inspector"))
  .settings(
    scalaVersion := scala3Version,
    libraryDependencies += "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.6.6"
  )

lazy val example = (project in file("example"))
  .settings(
    scalaVersion := scala3Version,
    libraryDependencies += "com.softwaremill.common" %% "tagging" % "2.3.2"
  )