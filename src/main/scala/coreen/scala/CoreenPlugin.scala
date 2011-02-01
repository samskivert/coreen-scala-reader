//
// $Id$

package coreen.scala

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
  val components = List(new CoreenPlugin.ReaderComponent(global))
}

object CoreenPlugin {
  class ReaderComponent (val global :Global) extends PluginComponent {
    val phaseName = "coreen"
    val runsAfter = List("refchecks") // TODO

    def newPhase (prev :Phase) :Phase = new StdPhase(prev) {
      def apply (unit :global.CompilationUnit) {
        println("Humble beginnings " + unit)
        // TODO!
      }
    }
  }
}
