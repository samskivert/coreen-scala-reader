import sbt._

class CoreenScalaReader (info :ProjectInfo) extends DefaultProject(info) /*with ProguardProject*/ {
  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  // val scalaj_collection = "org.scalaj" %% "scalaj-collection" % "1.0"

  override def filterScalaJars = false

  // // we need tools.jar in our classpath because we're building against scalac bits
  // val toolsJarPath = Path.fromFile(
  //   Path.fileProperty("java.home").asFile.getParent) / "lib" / "tools.jar"
  // override def unmanagedClasspath = super.unmanagedClasspath +++ toolsJarPath

  // // proguard plugin configurations
  // override def proguardInJars = super.proguardInJars +++ scalaLibraryPath
  // override def proguardLibraryJars = super.proguardLibraryJars +++ toolsJarPath
  // override def proguardOptions = List(
  //   "-dontnote scala.**,sun.tools.**,sun.applet.**",
  //   proguardKeepMain("coreen.scala.Main")
  // )
}
