import sbt._

object Dependencies {

  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "1.0.0-RC2"
  lazy val laikaCore = "org.planet42" %% "laika-core" % "0.8.0"

  lazy val commonsIO = "commons-io" % "commons-io" % "2.6"
  lazy val scalaCss = "com.github.japgolly.scalacss" %% "ext-scalatags" % "0.5.3"
  lazy val scalaTags = "com.lihaoyi" %% "scalatags" % "0.6.7"
  lazy val jettyServer = "org.eclipse.jetty" % "jetty-servlet" % "9.3.12.v20160915"
  lazy val jettyServlet = "org.eclipse.jetty" % "jetty-server" % "9.3.12.v20160915"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1"

}
