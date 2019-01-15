import Dependencies._

lazy val common = Seq(
  organization := "net.wayfarerx.oversite",
  scalaVersion := "2.12.6",
  version := "0.4.0",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ypartial-unification"),
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
    name := "oversite-ui",
    libraryDependencies ++= Seq(
      ScalaTags,
      ScalaCss
    )
  ).dependsOn(api)

lazy val model = (project in file("model")).
  settings(
    common,
    name := "oversite-model",
    libraryDependencies += LaikaCore
  ).dependsOn(api)

lazy val server = (project in file("server")).
  settings(
    common,
    name := "oversite-server",
    libraryDependencies ++= Seq(
      Scopt,
      Logback,
      Http4sDsl,
      Http4sBlazeServer
    )
  ).dependsOn(model)

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

lazy val plugin = (project in file("plugin")).
  settings(
    common,
    name := "sbt-oversite",
    libraryDependencies += ScalaTest % Test
  )
*/