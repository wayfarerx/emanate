import sbt._

object Dependencies {

  lazy val CommonsIO = "commons-io" % "commons-io" % "2.6"

  lazy val CatsEffect = "org.typelevel" %% "cats-effect" % "1.0.0-RC2"

  lazy val LaikaCore = "org.planet42" %% "laika-core" % "0.8.0"

  val Fs2Version = "1.0.0-M1"
  lazy val Fs2Core = "co.fs2" %% "fs2-core" % Fs2Version
  lazy val Fs2Io = "co.fs2" %% "fs2-io" % Fs2Version

  val Http4sVersion = "0.19.0-M1"
  lazy val Http4sDsl = "org.http4s" %% "http4s-dsl" % Http4sVersion
  lazy val Http4sBlazeServer = "org.http4s" %% "http4s-blaze-server" % Http4sVersion

  lazy val JettyServer = "org.eclipse.jetty" % "jetty-servlet" % "9.3.12.v20160915"
  lazy val JettyServlet = "org.eclipse.jetty" % "jetty-server" % "9.3.12.v20160915"

  lazy val ScalaCss = "com.github.japgolly.scalacss" %% "ext-scalatags" % "0.5.3"

  lazy val ScalaTags = "com.lihaoyi" %% "scalatags" % "0.6.7"

  lazy val ScalaTest = "org.scalatest" %% "scalatest" % "3.0.1"

}
