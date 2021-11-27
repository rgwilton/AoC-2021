val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name := "aoc-2021",
    version := "0.1.0",
    //scalacOptions := Seq("-Yexplicit-nulls"),
    scalaVersion := scala3Version,
    Compile / mainClass := Some("aoc.aoc"),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
