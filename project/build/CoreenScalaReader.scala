import sbt._

class CoreenScalaReader (info :ProjectInfo) extends DefaultProject(info) with ProguardProject {
  val specs = "org.scala-tools.testing" %% "specs"  % "1.6.7.1" % "test"
  // val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  // val scalaj_collection = "org.scalaj" %% "scalaj-collection" % "1.0"

  override def filterScalaJars = false

  override def localScala =
    List(defineScala("2.9.0-local", new java.io.File("/home/mdb/ops/scala-trunk/target/pack")))

  // proguard plugin configurations
  override def proguardInJars = super.proguardInJars +++ buildCompilerJar
  override def proguardLibraryJars = super.proguardLibraryJars +++ buildLibraryJar
  override def proguardOptions = List(
    "-dontnote scala.**",
    "-keep class ch.epfl.lamp.fjbg.** { *; }",
    "-keep class scala.tools.nsc.** { *; }",
    proguardKeepMain("coreen.scala.Main")
  )
}
