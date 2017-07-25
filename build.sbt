name := "scala-ml"

version := "0.3"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }
