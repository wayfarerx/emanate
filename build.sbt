import Dependencies._

lazy val common = Seq(
  organization := "net.wayfarerx.oversite",
  scalaVersion := "2.12.6",
  version := "0.1.34",
  scalacOptions ++= Seq("-Ypartial-unification"),
  libraryDependencies += ScalaTest % Test
)

lazy val api = (project in file("api")).
  settings(
    common,
    name := "oversite-api",
    libraryDependencies += CatsEffect
  )

lazy val ui = (project in file("ui")).
  settings(
    common,
    name := "oversite-ui"
  ).dependsOn(api)

lazy val model = (project in file("model")).
  settings(
    common,
    name := "oversite-model",
    libraryDependencies += LaikaCore
  ).dependsOn(api)
/*
lazy val generator = (project in file("generator")).
  settings(
    common,
    name := "oversite-generator",
    libraryDependencies ++= Seq(
      Slf4j,
      Fs2Core,
      Fs2Io
    )
  ).dependsOn(model)

lazy val server = (project in file("server")).
  settings(
    common,
    name := "oversite-server",
    libraryDependencies ++= Seq(
      Slf4j,
      Fs2Core,
      Fs2Io,
      Http4sDsl,
      Http4sBlazeServer
    )
  ).dependsOn(model)

lazy val plugin = (project in file("plugin")).
  settings(
    common,
    name := "sbt-oversite",
    libraryDependencies += ScalaTest % Test
  )
*/