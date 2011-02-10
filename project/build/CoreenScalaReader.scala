import sbt._

class CoreenScalaReader (info :ProjectInfo) extends DefaultProject(info) with ProguardProject {
  val specs = "org.scala-tools.testing" %% "specs"  % "1.6.7.1" % "test"
  // val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  // val scalaj_collection = "org.scalaj" %% "scalaj-collection" % "1.0"

  override def filterScalaJars = false

  override def localScala =
    List(defineScala("2.9.0-local", new java.io.File("/home/mdb/ops/scala-trunk/target/pack")))

  // TODO: we need scala-compiler.jar

  // we need tools.jar in our classpath because we're building against javac bits
  // val toolsJarPath = Path.fromFile(
  //   Path.fileProperty("java.home").asFile.getParent) / "lib" / "tools.jar"
  // override def unmanagedClasspath = super.unmanagedClasspath +++ toolsJarPath

  // proguard plugin configurations
  override def proguardInJars = super.proguardInJars +++ scalaLibraryPath
  // override def proguardLibraryJars = super.proguardLibraryJars +++ toolsJarPath
  override def proguardOptions = List(
    "-dontnote scala.**",
    proguardKeepMain("coreen.scala.Main")
  )
}
