val dottyVersion = "3.0.0-M2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "adventofcode2020",
    version := "1.0.0",
    scalaVersion := dottyVersion
  )
