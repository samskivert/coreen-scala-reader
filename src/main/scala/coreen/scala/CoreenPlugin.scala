//
// $Id$

package coreen.scala

import scala.collection.mutable.Buffer
import scala.xml.Elem
import scala.reflect.generic.Trees

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
    import global._ // for Tree, Traverser, CompilationUnit, Apply, etc.

    val phaseName = "coreen"
    val runsAfter = List("refchecks") // TODO

    // we write our <compunit> elements here so Reader can access them
    val unitelems = Buffer[Elem]()

    def newPhase (prev :Phase) :Phase = new StdPhase(prev) {
      def apply (unit :CompilationUnit) {
        println("Processing " + unit + "...")
        val conv = newConverter
        conv.traverse(unit.body)
        unitelems += <compunit name={unit.source.file.name}>{conv.buf}</compunit>
      }
    }

    def newConverter = new Traverser {
      var buf = Buffer[Elem]()

      override def traverse (tree :Tree) :Unit = tree match {
        case PackageDef(pid, stats) => {
          buf += <def name={pid.toString} kind="module">
          {capture(super.traverse(tree))}
          </def>
        }
        case ClassDef(mods, name, tparams, impl) => {
          buf += <def name={name.toString} kind="type">
          {capture(super.traverse(tree))}
          </def>
        }
        case ModuleDef(mods, name, impl) => {
          buf += <def name={name.toString} kind="module">
          {capture(super.traverse(tree))}
          </def>
        }
        case ValDef(mods, name, tpt, rhs) => {
          println("val mods " + name + " => " + mods)
          buf += <def name={name.toString} kind="term">
          {capture(super.traverse(tree))}
          </def>
        }
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
          println("def mods " + name + " => " + mods)
          buf += <def name={name.toString} kind="func">
          {capture(super.traverse(tree))}
          </def>
        }
        case Apply(fun, args) => {
          println("traversing application of "+ fun)
          super.traverse(tree)
        }
        case _ => super.traverse(tree)
      }

      private def capture (call : =>Unit) :Seq[Elem] = {
        val obuf = buf
        buf = Buffer[Elem]()
        call
        val nbuf = buf
        buf = obuf
        nbuf
      }
    }
  }
}
