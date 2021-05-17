name := "scalaml-algo"

version := "0.6"

scalaVersion := "2.13.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

assemblyJarName in assembly := name.value + ".jar"
