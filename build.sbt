import Dependencies._

lazy val common = Seq(
  organization := "net.wayfarerx",
  scalaVersion := "2.12.6",
  version := "0.1.0-SNAPSHOT"
)

lazy val api = (project in file("api")).
  settings(
    common,
    name := "oversite-api",
    libraryDependencies ++= Seq(
      catsEffect,
      scalaTest % Test
    )
  )

lazy val util = (project in file("util")).
  settings(
    common,
    name := "oversite-util",
    libraryDependencies ++= Seq(
      scalaTest % Test
    )
  ).dependsOn(api)

lazy val model = (project in file("model")).
  settings(
    common,
    name := "oversite-model",
    libraryDependencies ++= Seq(
      laikaCore,
      scalaTest % Test
    )
  ).dependsOn(api)

lazy val generator = (project in file("generator")).
  settings(
    common,
    name := "oversite-generator",
    libraryDependencies += scalaTest % Test
  ).dependsOn(model)

lazy val server = (project in file("server")).
  settings(
    common,
    name := "oversite-server",
    libraryDependencies ++= Seq(
      commonsIO,
      jettyServer,
      jettyServlet,
      scalaTest % Test
    )
  ).dependsOn(model)

lazy val plugin = (project in file("plugin")).
  settings(
    common,
    name := "sbt-oversite",
    libraryDependencies += scalaTest % Test
  )
