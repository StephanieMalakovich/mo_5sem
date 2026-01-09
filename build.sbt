ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "2.1.0",
  "org.scalanlp" %% "breeze-viz" % "2.1.0"
)

resolvers += "Sonatype Releases" at
  "https://oss.sonatype.org/content/repositories/releases/"

lazy val root = (project in file("."))
  .settings(
    name := "metod_op 1"
  )