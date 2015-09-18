// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Labs.

lazy val commonSettings = Seq(
  scalaVersion := "2.11.7"
)

lazy val scalaReflect = Def.setting { "org.scala-lang" % "scala-reflect" % scalaVersion.value }

lazy val root = (project in file(".")).
  dependsOn(macros).
  settings(commonSettings: _*).
  settings(
    name := "Kse",
    version := "0.3.0",
    scalaVersion := "2.11.7",
    mappings in (Compile, packageBin) ++= mappings.in(macros, Compile, packageBin).value,
    mappings in (Compile, packageSrc) ++= mappings.in(macros, Compile, packageSrc).value,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test"
  )

lazy val macros = (project in file("macros")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies += scalaReflect.value,
    publish := {},
    publishLocal := {}
  )
