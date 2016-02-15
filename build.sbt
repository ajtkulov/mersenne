name := "mersenne"

version := "1.0"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

parallelExecution in Test := false

val myProject = (project in file("."))

libraryDependencies += "com.lihaoyi" % "ammonite-repl" % "0.5.2" % "test" cross CrossVersion.full

initialCommands in (Test, console) := """ammonite.repl.Main.run("")"""
