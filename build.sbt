name := "scalaml-algo"

version := "0.4"

scalaVersion := "2.12.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

assemblyJarName in assembly := name.value + ".jar"
