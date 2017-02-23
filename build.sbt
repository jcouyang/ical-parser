name := "ical-parser"

scalaVersion := "2.11.8"
sbtVersion := "0.13.12"

val specs2Version = "3.8.6"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.2")
addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")
