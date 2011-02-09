//
// $Id$

package coreen.scala

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin

/**
 * The main entry point for the Coreen scalac plugin.
 */
class CoreenPlugin (val global :Global) extends Plugin
{
  val name = "Coreen Scala Reader"
  val runsAfter = List("refchecks") // TODO
  val description = "Generates Coreen metadata from Scala source."
  val components = List(new TranslatorComponent(global))
}
