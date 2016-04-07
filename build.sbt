// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Labs.

/////////////////////////////////
// Sonatype publishing section //
/////////////////////////////////
publishMavenStyle := true
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
publishArtifact in Test := false
pomExtra := (
  <url>http://www.github.com/ichoran</url>
  <licenses>
    <license><name>BSD 3-clause</name><url>https://opensource.org/licenses/BSD-3-Clause</url><distribution>repo</distribution></license>
  </licenses>
  <scm>
    <url>git@github.com:ichoran/kse.git</url>
    <connection>scm:git:git@github.com:ichoran/kse.git</connection>
  </scm>
  <developers>
    <developer><id>ichoran</id><name>Rex Kerr</name></developer>
  </developers>
)


///////////////////////////
// Actual build settings //
///////////////////////////

lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  version := "0.4-SNAPSHOT",
  scalacOptions += "-feature",
  scalacOptions += "-deprecation"
)

lazy val scalaReflect = Def.setting { "org.scala-lang" % "scala-reflect" % scalaVersion.value }
lazy val jUnit = "com.novocode" % "junit-interface" % "0.9"

lazy val root = (project in file(".")).
  dependsOn(macros % "compile-internal, test-internal").
  settings(commonSettings: _*).
  settings(
    name := "Kse",
    mappings in (Compile, packageBin) ++= mappings.in(macros, Compile, packageBin).value,
    mappings in (Compile, packageSrc) ++= mappings.in(macros, Compile, packageSrc).value,
    libraryDependencies += jUnit % Test
  )

lazy val macros = (project in file("macros")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies += scalaReflect.value,
    publish := {},
    publishLocal := {}
  )
