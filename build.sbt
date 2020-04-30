// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015-2020 Rex Kerr and Calico Labs.

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
    <license>
      <name>BSD 3-clause</name>
      <url>https://opensource.org/licenses/BSD-3-Clause</url>
      <distribution>repo</distribution>
      <comments>Copyright 2011-2019 by Rex Kerr, HHMI Janelia, UC San Francisco, and Calico Life Sciences.</comments>
    </license>
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
  scalaVersion := "2.12.11",
  version := "0.11-SNAPSHOT",
  scalacOptions += "-feature",
  scalacOptions += "-deprecation"
)

lazy val scalaReflect = Def.setting { "org.scala-lang" % "scala-reflect" % scalaVersion.value }
lazy val jUnit = "com.novocode" % "junit-interface" % "0.9"

lazy val kse = (project in file(".")).
  dependsOn(macros % "compile-internal, test-internal").
  settings(commonSettings: _*).
  settings(
    organization := "com.github.ichoran",
    libraryDependencies += jUnit % Test,
    mappings in (Compile, packageBin) ++= mappings.in(macros, Compile, packageBin).value,
    mappings in (Compile, packageSrc) ++= mappings.in(macros, Compile, packageSrc).value
  )

lazy val macros = project settings (
  commonSettings,
  libraryDependencies += scalaReflect.value,
  publish := {},
  publishLocal := {} 
)
