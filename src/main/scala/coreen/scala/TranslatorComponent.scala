//
// $Id$

package coreen.scala

import scala.collection.mutable.Buffer
import scala.reflect.generic.{Trees, Flags}
import scala.xml.Elem

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.PluginComponent

/**
 * Traverses a Scala AST and translates it into the Coreen XML format.
 */
class TranslatorComponent (val global :Global) extends PluginComponent
{
  import global._ // for Tree, Traverser, CompilationUnit, Apply, etc.

  val phaseName = "coreen"
  val runsAfter = List("refchecks") // TODO

  // we write our <compunit> elements here so Reader can access them
  val unitelems = Buffer[Elem]()

  def newPhase (prev :Phase) :Phase = new StdPhase(prev) {
    def apply (unit :CompilationUnit) {
      println("Processing " + unit + "...")
      val trans = newTranslator
      trans.traverse(unit.body)
      unitelems += <compunit name={unit.source.file.name}>{trans.buf}</compunit>
    }
  }

  def newTranslator = new Traverser {
    var buf = Buffer[Elem]()

    // TODO: id={_curid} start={_text.indexOf(pname, _ctx.curunit.pos).toString}
    override def traverse (tree :Tree) :Unit = tree match {
      case PackageDef(pid, stats) => {
        buf += <def name={pid.toString} kind="module" flavor="none" access="public">
        {capture(super.traverse(tree))}
        </def>
      }
      case ClassDef(mods, name, tparams, impl) => {
        buf += <def name={name.toString} kind="type" flavor="none" access={access(mods)}>
        {capture(super.traverse(tree))}
        </def>
      }
      case ModuleDef(mods, name, impl) => {
        buf += <def name={name.toString} kind="module" flavor="none" access={access(mods)}>
        {capture(super.traverse(tree))}
        </def>
      }
      case ValDef(mods, name, tpt, rhs) => {
        println("val mods " + name + " => " + mods)
        buf += <def name={name.toString} kind="term" flavor="none" access={access(mods)}>
        {capture(super.traverse(tree))}
        </def>
      }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
        val isCtor = (name == nme.CONSTRUCTOR)
          val flavor = if (isCtor) "constructor"
                       else "method" // TODO
        // println("def mods " + name + " => " + mods)
        buf += <def name={name.toString} kind="func" flavor={flavor} access={access(mods)}>
        {capture(super.traverse(tree))}
        </def>
      }
      case Apply(fun, args) => {
        println("traversing application of "+ fun)
        super.traverse(tree)
      }
      case _ => super.traverse(tree)
    }

    private def access (mods :Modifiers) =
      if (mods hasFlag Flags.PROTECTED) "protected"
      else if (mods hasFlag Flags.PRIVATE) "private"
      else "public"

    private def withId (id :String)(block : =>Unit) {
      val oid = _curid
      _curid = id
      block
      _curid = oid
    }

    private def capture (call : =>Unit) :Seq[Elem] = {
      val obuf = buf
      buf = Buffer[Elem]()
      call
      val nbuf = buf
      buf = obuf
      nbuf
    }

    private var _curid :String = _
  }
}
