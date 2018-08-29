import sbt._

object Dependencies {

  lazy val CommonsIO = "commons-io" % "commons-io" % "2.6"
  lazy val CatsEffect = "org.typelevel" %% "cats-effect" % "1.0.0-RC2"

  lazy val LaikaCore = "org.planet42" %% "laika-core" % "0.8.0"

  lazy val JettyServer = "org.eclipse.jetty" % "jetty-servlet" % "9.3.12.v20160915"
  lazy val JettyServlet = "org.eclipse.jetty" % "jetty-server" % "9.3.12.v20160915"

  lazy val ScalaCss = "com.github.japgolly.scalacss" %% "ext-scalatags" % "0.5.3"
  lazy val ScalaTags = "com.lihaoyi" %% "scalatags" % "0.6.7"

  lazy val ScalaTest = "org.scalatest" %% "scalatest" % "3.0.1"

}
