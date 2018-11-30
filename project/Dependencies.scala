import sbt._

object Dependencies {

  lazy val CatsEffect = "org.typelevel" %% "cats-effect" % "1.0.0"

  lazy val LaikaCore = "org.planet42" %% "laika-core" % "0.9.0"

  lazy val ScalaCss = "com.github.japgolly.scalacss" %% "ext-scalatags" % "0.5.3"

  lazy val ScalaTags = "com.lihaoyi" %% "scalatags" % "0.6.7"

  lazy val Slf4j = "org.slf4j" % "slf4j-api" % "1.7.25"

  val LogbackVersion = "1.2.3"
  lazy val LogbackClassic = "ch.qos.logback" % "logback-classic" % LogbackVersion
  lazy val LogbackCore = "ch.qos.logback" % "logback-core" % LogbackVersion

  val Fs2Version = "1.0.0-M1"
  lazy val Fs2Core = "co.fs2" %% "fs2-core" % Fs2Version
  lazy val Fs2Io = "co.fs2" %% "fs2-io" % Fs2Version

  val Http4sVersion = "0.19.0-M1"
  lazy val Http4sDsl = "org.http4s" %% "http4s-dsl" % Http4sVersion
  lazy val Http4sBlazeServer = "org.http4s" %% "http4s-blaze-server" % Http4sVersion

  lazy val ScalaTest = "org.scalatest" %% "scalatest" % "3.0.5"

}
