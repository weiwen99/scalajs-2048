ThisBuild / name := "simple2048"
ThisBuild / organization := "simple2048"
ThisBuild / scalaVersion := "2.13.6"

lazy val root = project
  .in(file("."))
  .settings(name := (ThisBuild / name).value)
  .settings(version := (ThisBuild / version).value)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(UniversalPlugin)
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaJSUseMainModuleInitializer := true)
  .settings(jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv())
  .settings(Compile / mainClass := Some("simple.webapp.simple2048.Main"))

libraryDependencies ++= List(
  "org.scala-js" %%% "scalajs-dom" % "1.1.0"
)
