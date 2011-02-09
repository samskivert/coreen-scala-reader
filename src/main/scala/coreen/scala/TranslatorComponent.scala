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

    // TODO: start={...} bodyStart={...} bodyEnd={...}
    override def traverse (tree :Tree) :Unit = tree match {
      case PackageDef(pid, stats) => {
        withId(pid.toString) {
          buf += <def id={_curid} name={pid.toString} kind="module" flavor="none" access="public">
          {capture(super.traverse(tree))}
          </def>
        }
      }

      case ClassDef(mods, name, tparams, impl) => {
        withId(name.toString) {
          buf += <def id={_curid} name={name.toString} kind="type" flavor="none"
                      access={access(mods)}>
          {capture(super.traverse(tree))}
          </def>
        }
      }

      case ModuleDef(mods, name, impl) => {
        withId(name.toString) {
          buf += <def id={_curid} name={name.toString} kind="module" flavor="none"
                      access={access(mods)}>
          {capture(super.traverse(tree))}
          </def>
        }
      }

      case ValDef(mods, name, tpt, rhs) => {
        println("val mods " + name + " => " + mods)
        withId(name.toString) {
          buf += <def id={_curid} name={name.toString} kind="term" flavor="none"
                      access={access(mods)}>
          {capture(super.traverse(tree))}
          </def>
        }
      }

      case DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
        val isCtor = (name == nme.CONSTRUCTOR)
        // println("def mods " + name + " => " + mods + ", oname " + currentOwner.name)
        val flavor = if (isCtor) "constructor" else "method" // TODO
        val dname = if (isCtor) currentOwner.name.toString // owning class name
                    else name.toString
        withId(dname) {
          buf += <def id={_curid} name={dname} kind="func" flavor={flavor} access={access(mods)}>
          {capture(super.traverse(tree))}
          </def>
        }
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

    private def joinDefIds (first :String, second :String) = {
      val sep = if (!first.isEmpty) " " else ""
      first + sep + second
    }

    // private def withCtx (ctx :Context)(block : =>Unit) {
    //   val octx = _ctx
    //   _ctx = ctx
    //   block
    //   _ctx = octx
    // }

    private def withId (id :String)(block : =>Unit) {
      val oid = _curid
      _curid = joinDefIds(_curid, id)
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

    // case class Context (curunit :JCCompilationUnit,
    //                     curclass :JCClassDecl,
    //                     curmeth :JCMethodDecl,
    //                     curdoc :DefDoc)
    // private var _ctx :Context = Context(null, null, null, null)

    private var _curid :String = ""
  }
}
