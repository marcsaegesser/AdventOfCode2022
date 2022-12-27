val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "AdventOfCode2022",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    
  )
