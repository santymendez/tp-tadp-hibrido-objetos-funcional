
ThisBuild / organization := "ar.edu.utn.frba.tadp"

ThisBuild / scalaVersion := "3.3.1"

ThisBuild / version := "1.0"

name := "tp-hibrido-objetos-funcional"

resolvers += "jitpack" at "https://jitpack.io" // Add JitPack repository, to download dependency from github

libraryDependencies += "com.github.tadp-utn-frba" % "tp-dibujador-poc" % "-bcc98c0d23-1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"