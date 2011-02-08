//
// $Id$

package coreen.scala

import java.io.File
import java.net.URLClassLoader

import scala.tools.nsc.{Settings, Global}
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.{SourceFile, BatchSourceFile, ClassPath}
import scala.xml.Elem

/**
 * Provides an API for converting Scala source to name-resolved source.
 */
object Reader
{
  /**
   * Processes a list of source files.
   * @return a list of {@code <compunit>} elements containing their defs and uses.
   */
  def process (files :Seq[File], classpath :Seq[File]) :Iterable[Elem] = {
    process0(files.map(f => new BatchSourceFile(AbstractFile.getFile(f))).toList, classpath)
  }

  /**
   * Processes the supplied text as a source file with the specified name.
   * @return a {@code <compunit>} element containing the source code's defs and uses.
   */
  def process (filename :String, content :String) :Elem =
    process0(List(new BatchSourceFile(filename, content)), List()) head

  /**
   * Processes the supplied (filename, source text) pairs.
   * @return a list of {@code <compunit>} elements containing the source code's defs and uses.
   */
  def process (sources :List[(String, String)]) :Iterable[Elem] =
    process0(sources.map { case (name, cont) => new BatchSourceFile(name, cont) }, List())

  def process0 (sources :List[SourceFile], classpath :Seq[File]) :Iterable[Elem] = {
    val settings = new Settings

    // TODO: this is all SBT specific

    // we have to set up the classpath for the compiler manually
    val loader = getClass.getClassLoader.asInstanceOf[URLClassLoader]
    println(loader)
    println(loader.getParent)
    println(getField(loader.getParent, "parentA").asInstanceOf[ClassLoader].getParent)
    println(getField(loader.getParent, "parentB").asInstanceOf[URLClassLoader].getURLs.toList)
    val entries = loader.getURLs map(_.getPath)
    // annoyingly, the Scala library is not in our classpath, so we have to add it manually
    val sclpath = entries find(_.endsWith("scala-compiler.jar")) map(
      _.replaceAll("scala-compiler.jar", "scala-library.jar"))
    settings.classpath.value = ClassPath.join((entries ++ sclpath) : _*)

    val compiler = new Global(settings, new ConsoleReporter(settings))
    // TODO: stop at a particular phase, activate plugins
    new compiler.Run() compileSources(sources)
    Nil
  }

  def getField (obj :AnyRef, field :String) = {
    val f = obj.getClass.getDeclaredFields find(_.getName == field) getOrElse(
      throw new Exception("Missing field " + field))
    f.setAccessible(true)
    f.get(obj)
  }
}
