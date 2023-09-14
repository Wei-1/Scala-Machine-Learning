name := "scalaml-algo"

version := "0.6"

scalaVersion := "2.13.12"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"

assemblyJarName in assembly := name.value + ".jar"
