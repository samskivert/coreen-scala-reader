//
// $Id$

package coreen.scala

import java.io.File
import java.util.Date
import java.lang.System.out

import scala.io.Source
import scala.xml.{Node, PrettyPrinter}

/**
 * Main entry point for Scala Reader.
 */
object Main
{
  def main (args :Array[String]) {
    if (args.length < 2) {
      die("Usage: coreen.scala.Main project_root_dir last_mod_stamp [src_dir ...]")
    }

    // extract and parse our arguments
    val (root, lastMod, filters) = (new File(args(0)), args(1).toLong, args drop(2))

    // set up a filter function if any path prefixes were provided
    val filter = if (filters.isEmpty) (path :String) => true
                 else mkFilter(args(0), filters) _

    // scan the project directory for source and jar files
    val files = collectFiles(root, filter)

    // try to find our project dependencies via a pom.xml file or a .coreen file, and fall back to
    // scanning the project directories for all jar files in the absence of those
    val jars = locateJarsViaSBT(root).getOrElse(
      locateJarsViaMaven(root).getOrElse(
        locateJarsViaDotCoreen(root).getOrElse(
          files.getOrElse("jar", List()))))
    out.println("Using classpath:")
    for (j <- jars) out.println("  " + j)

    // allow pretty printed output for debugging
    val print = if (java.lang.Boolean.getBoolean("pretty")) {
      val pp = new PrettyPrinter(999, 2)
      (e :Node) => out.println(pp.format(e))
    } else {
      (e :Node) => out.println(e)
    }

    // process only those sources newer than our supplied last modified cutoff
    val sources = files.get("scala").getOrElse(Nil).filter(_.lastModified >= lastMod)
    if (sources.isEmpty) {
      out.println("No .scala files modified after " + new Date(lastMod) + " in " + root)
    } else {
      out.println("Compiling " + sources.size + " Scala source files...")
      Reader.process(sources, jars) foreach(print)
    }
  }

  def mkFilter (root :String, prefixes :Seq[String])(path :String) = path.startsWith(root) && {
    val relpath = stripFS(path.substring(root.length))
    prefixes exists(pre => relpath.startsWith(pre) || pre.startsWith(relpath))
  }

  def locateJarsViaSBT (root :File) :Option[Seq[File]] = {
    val props = new File(new File(root, "project"), "build.properties")
    if (!props.exists) None
    else collectFiles(root, mkFilter(root.getPath, List("lib", "lib_managed"))).get("jar")
  }

  def locateJarsViaMaven (root :File) :Option[Seq[File]] = {
    val pom = new File(root, "pom.xml")
    if (!pom.exists) None else {
      try {
        val p = Runtime.getRuntime.exec(Array("mvn", "dependency:build-classpath"), null, root)
        // the last line of output that does not start with '[' should be the classpath
        Source.fromInputStream(p.getInputStream).getLines.filterNot(
          _.startsWith("[")).toList.lastOption.map(toFiles)
      } catch {
        case e => out.println("Failure resolving Maven classpath " + e); None
      }
    }
  }

  def locateJarsViaDotCoreen (root :File) :Option[Seq[File]] = {
    val dc = new File(root, ".coreen")
    if (!dc.exists) None else readConfig(dc).get("cpcommand") flatMap { cpc =>
      try {
        val p = Runtime.getRuntime.exec(cpc, null, root)
        // assume the first line of output is the classpath in question
        Source.fromInputStream(p.getInputStream).getLines.toSeq.headOption.map(toFiles)
      } catch {
        case e => out.println("Failure resolving classpath via '" + cpc + "': " + e); None
      }
    }
  }

  /** Reads our .coreen configuration file into a map of key/value pairs. */
  def readConfig (cfile :File) :Map[String,String] = {
    def toPair (line :String) = line.indexOf("=") match {
      case -1 => None
      case eidx => Some(line.substring(0, eidx).trim, line.substring(eidx+1).trim)
    }
    Source.fromFile(cfile).getLines.toSeq.flatMap(toPair).toMap
  }

  /** Converts a classpath string (i.e. "foo/bar.jar:dist/classes") to a Seq[File]. */
  def toFiles (path :String) :Seq[File] = path.split(File.pathSeparator).map(new File(_)).toSeq

  def collectFiles (file :File, filter :(String => Boolean)) :Map[String,List[File]] = {
    def suffix (name :String) = name.substring(name.lastIndexOf(".")+1)
    def collect (file :File) :List[(String,File)] = {
      if (file.isDirectory) {
        if (!isSymlink(file) && filter(file.getPath)) file.listFiles.toList flatMap(collect)
        else List()
      } else suffix(file.getName) match {
        case "scala" => List(("scala", file.getCanonicalFile))
        case "jar" => List(("jar", file.getCanonicalFile))
        case _ => List()
      }
    }
    collect(file) groupBy(_._1) mapValues(_.map(_._2) distinct)
  }

  def stripFS (path :String) = if (path.startsWith(File.separator)) path.substring(1) else path

  // a flaky and expensive means for detecting if a file is a symlink that works for our uses
  private def isSymlink (file :File) = file.getCanonicalPath != file.getAbsolutePath

  private def die (msg :String) = {
    System.err.println(msg)
    /*sys.*/exit(255)
    /*sys.*/error("Not reached")
  }
}
