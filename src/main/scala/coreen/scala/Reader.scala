//
// $Id$

package coreen.scala

import java.io.File
import java.net.URLClassLoader

import scala.xml.Elem

import scala.tools.nsc.interactive.RangePositions
import scala.tools.nsc.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.{SourceFile, BatchSourceFile, ClassPath}
import scala.tools.nsc.{Settings, Global}

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
    val entries = loader.getURLs map(_.getPath)
    // annoyingly, the Scala library is not in our classpath, so we have to add it manually
    val sclpath = entries find(_.endsWith("scala-compiler.jar")) map(
      _.replaceAll("scala-compiler.jar", "scala-library.jar"))
    settings.classpath.value = ClassPath.join((entries ++ sclpath) : _*)

    // save class files to a virtual directory in memory
    settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))

    val compiler = new ReaderCompiler(settings)
    new compiler.Run() compileSources(sources)
    compiler.rcomp.unitelems
  }

  class ReaderCompiler (settings :Settings) extends Global(settings, new ConsoleReporter(settings))
    with RangePositions
  {
    val rcomp = new TranslatorComponent(this)
    override protected def computeInternalPhases () {
      super.computeInternalPhases
      phasesSet += rcomp
    }
  }
}
