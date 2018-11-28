name := """puzzles"""

version := "1.0"

scalaVersion := "2.12.4"

resourceDirectory in Compile := baseDirectory.value / "resources"

// Change this to another test framework if you prefer
libraryDependencies += "com.lihaoyi" %% "fansi" % "0.2.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

