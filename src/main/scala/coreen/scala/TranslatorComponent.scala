//
// $Id$

package coreen.scala

import scala.collection.mutable.Buffer
import scala.reflect.generic.{Trees, Flags}
import scala.xml.Elem

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.util.RangePosition

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
      unitelems += <compunit src={unit.source.file.path}>{trans.buf}</compunit>
    }
  }

  def newTranslator = new Traverser {
    var buf = Buffer[Elem]()

    // TODO: start={...} bodyStart={...} bodyEnd={...}
    override def traverse (tree :Tree) :Unit = tree match {
      case t @ PackageDef(pid, stats) => {
        withId(pid.toString) {
          buf += mkDef(pid.toString, "module", "none", "public", t.pos,
                       capture(super.traverse(tree)))
        }
      }

      case t @ ClassDef(mods, name, tparams, impl) => {
        withId(name.toString) {
          buf += mkDef(name.toString, "type", "none", access(mods), t.pos,
                       capture(super.traverse(tree)))
        }
      }

      case t @ ModuleDef(mods, name, impl) => {
        withId(name.toString) {
          buf += mkDef(name.toString, "module", "none", access(mods), t.pos,
                       capture(super.traverse(tree)))
        }
      }

      case t @ ValDef(mods, name, tpt, rhs) => {
        // a 'val foo' turns into 'def "foo"' and 'val "foo "'; workaround this for now
        val ename = name.toString.replace(' ', '_')
        withId(ename) {
          buf += mkDef(ename, "term", "none", access(mods), t.pos,
                       capture(super.traverse(tree)))
        }
      }

      case t @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
        val isCtor = (name == nme.CONSTRUCTOR)
        // println("def mods " + name + " => " + mods + ", oname " + currentOwner.name)
        val flavor = if (isCtor) "constructor" else "method" // TODO
        val dname = if (isCtor) currentOwner.name.toString // owning class name
                    else name.toString
        withId(dname) {
          buf += mkDef(dname, "func", flavor, access(mods), t.pos, capture(super.traverse(tree)))
        }
      }

      // case Apply(fun, args) => {
      //   println("traversing application of "+ fun)
      //   super.traverse(tree)
      // }

      case _ => super.traverse(tree)
    }

    private def mkDef (name :String, kind :String, flavor :String, access :String, pos :Position,
                       body :Seq[Elem]) = {
      val (start, point, end) = pos match {
        case rp :RangePosition => (rp.start, rp.point, rp.end)
        case _ => (-1, pos.point, -1)
      }
      <def id={_curid} name={name} kind={kind} flavor={flavor} access={access}
           start={point.toString} bodyStart={start.toString} bodyEnd={end.toString}>
        {body}
      </def>
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
