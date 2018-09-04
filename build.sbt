import Dependencies._

lazy val common = Seq(
  organization := "net.wayfarerx.oversite",
  scalaVersion := "2.12.6",
  version := "0.1.7",
  scalacOptions ++= Seq("-Ypartial-unification")
)

lazy val api = (project in file("api")).
  settings(
    common,
    name := "oversite-api",
    libraryDependencies ++= Seq(
      CatsEffect,
      ScalaTest % Test
    )
  )

lazy val utils = (project in file("utils")).
  settings(
    common,
    name := "oversite-utils",
    libraryDependencies ++= Seq(
      ScalaTags,
      ScalaCss,
      ScalaTest % Test
    )
  ).dependsOn(api)

lazy val model = (project in file("model")).
  settings(
    common,
    name := "oversite-model",
    libraryDependencies ++= Seq(
      LaikaCore,
      ScalaTest % Test
    )
  ).dependsOn(api)

lazy val generator = (project in file("generator")).
  settings(
    common,
    name := "oversite-generator",
    libraryDependencies += ScalaTest % Test
  ).dependsOn(model)

lazy val server = (project in file("server")).
  settings(
    common,
    name := "oversite-server",
    libraryDependencies ++= Seq(
      CommonsIO,
      Fs2Core,
      Fs2Io,
      Http4sDsl,
      Http4sBlazeServer,
      JettyServer,
      JettyServlet,
      ScalaTest % Test
    )
  ).dependsOn(model)

lazy val plugin = (project in file("plugin")).
  settings(
    common,
    name := "sbt-oversite",
    libraryDependencies += ScalaTest % Test
  )
