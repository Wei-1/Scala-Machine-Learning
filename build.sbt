name := "scalaml-algo"

version := "0.6"

scalaVersion := "3.9.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % "test"

assemblyJarName in assembly := name.value + ".jar"
