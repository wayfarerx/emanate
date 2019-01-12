import sbt._

object Dependencies {

  lazy val ScalaTest = "org.scalatest" %% "scalatest" % "3.0.5"

  lazy val CatsEffect = "org.typelevel" %% "cats-effect" % "1.1.0"

  lazy val LaikaCore = "org.planet42" %% "laika-core" % "0.10.0"

  lazy val ScalaTags = "com.lihaoyi" %% "scalatags" % "0.6.7"

  lazy val ScalaCss = "com.github.japgolly.scalacss" %% "ext-scalatags" % "0.5.3"

  lazy val Scopt = "com.github.scopt" %% "scopt" % "3.7.1"

  lazy val Logback = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val Http4sVersion = "0.19.0"
  lazy val Http4sDsl = "org.http4s" %% "http4s-dsl" % Http4sVersion
  lazy val Http4sBlazeServer = "org.http4s" %% "http4s-blaze-server" % Http4sVersion

}
