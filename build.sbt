import Dependencies._

lazy val common = Seq(
  organization := "net.wayfarerx.oversite",
  scalaVersion := "2.12.7",
  version := "0.6.1",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ypartial-unification"),
  libraryDependencies += ScalaTest % Test,
  publishMavenStyle := false,
  publishTo := Some(Resolver.url(
    "WayfarerX Releases",
    url("s3://software.wayfarerx.net/releases")
  )(Resolver.ivyStylePatterns))
)

lazy val root = (project in file("."))
  .settings(
    skip in publish := true
  ).aggregate(api, ui, model, server, generator)

lazy val api = (project in file("api"))
  .settings(
    common,
    name := "oversite-api",
    libraryDependencies += CatsEffect
  )

lazy val ui = (project in file("ui"))
  .settings(
    common,
    name := "oversite-ui",
    libraryDependencies ++= Seq(
      ScalaTags,
      ScalaCss
    )
  ).dependsOn(api)

lazy val model = (project in file("model"))
  .settings(
    common,
    name := "oversite-model",
    libraryDependencies += LaikaCore
  ).dependsOn(api)

lazy val server = (project in file("server"))
  .settings(
    common,
    name := "oversite-server",
    libraryDependencies ++= Seq(
      Scopt,
      Logback,
      Http4sDsl,
      Http4sBlazeServer
    )
  ).dependsOn(model, ui % Test)

lazy val generator = (project in file("generator"))
  .settings(
    common,
    name := "oversite-generator",
    libraryDependencies ++= Seq(
      Scopt,
      Logback,
      CommonsIO
    )
  ).dependsOn(model, ui % Test)