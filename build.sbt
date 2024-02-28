name := "scalaml-algo"

version := "0.6"

scalaVersion := "2.13.13"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"

assemblyJarName in assembly := name.value + ".jar"
