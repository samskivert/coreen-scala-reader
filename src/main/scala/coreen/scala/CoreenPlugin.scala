//
// $Id$

package coreen.scala

import scala.collection.mutable.Buffer
import scala.xml.Elem

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

/**
 * The main entry point for the Coreen scalac plugin.
 */
class CoreenPlugin (val global :Global) extends Plugin
{
  val name = "Coreen Scala Reader"
  val runsAfter = List("refchecks") // TODO
  val description = "Generates Coreen metadata from Scala source."
  val components = CoreenPlugin.components(global)
}

object CoreenPlugin {
  def components (global :Global) =
    List(new CoreenPlugin.ReaderComponent(global))

  class ReaderComponent (val global :Global) extends PluginComponent {
    val phaseName = "coreen"
    val runsAfter = List("refchecks") // TODO

    // we write our <compunit> elements here so Reader can access them
    val unitelems = Buffer[Elem]()

    def newPhase (prev :Phase) :Phase = new StdPhase(prev) {
      def apply (unit :global.CompilationUnit) {
        println("Humble beginnings " + unit)
      }
    }
  }
}
