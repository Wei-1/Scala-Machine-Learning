name := "scalaml-algo"

version := "0.6"

scalaVersion := "2.13.15"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

assemblyJarName in assembly := name.value + ".jar"
