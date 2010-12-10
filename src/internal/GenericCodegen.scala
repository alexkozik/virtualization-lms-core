package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}

trait GenericCodegen extends Scheduling {
  val IR: Expressions
  import IR._

  def kernelFileExt = ""
  def emitKernelHeader(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {}
  def emitKernelFooter(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {}
  
    // Initializer
  def init(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultIsVar: Boolean): Unit = {}

  // optional type remapping (default is identity)
  def remap[A](m: Manifest[A]) : String = m.toString

  // exception handler
  def exceptionHandler(outFile:File, kstream:PrintWriter): Unit = {
      kstream.close()
      outFile.delete
  }
  
  def emitBlock(y: Exp[_])(implicit stream: PrintWriter): Unit = {
    val deflist = buildScheduleForResult(y)
    
    for (TP(sym, rhs) <- deflist) {
      emitNode(sym, rhs)
    }
  }

  def getBlockResult[A](s: Exp[A]): Exp[A] = s
  
  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Unit = {
    throw new Exception("don't know how to generate code for: " + rhs)
  }

  //def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit
  
  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit
      
  def quote(x: Exp[_]) : String = x match {
    case Const(s: String) => "\""+s+"\""
    case Const(null) => "null" // why is null getting lifted now? something to do with Equal
    case Const(z) => z.toString
    case Sym(n) => "x"+n
    case External(s: String, args: List[Exp[Any]]) => s.format(args map (quote(_)) : _*)
    case null => "null"
    case _ => throw new RuntimeException("could not quote " + x)
  }

  def getFreeVarBlock(start: Exp[_], local: List[Sym[_]]): List[Sym[_]] = { throw new Exception("Method getFreeVarBlock should be overriden.") }
  def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = { throw new Exception("Method getFreeVarNode should be overriden.") }

  def hasMetaData: Boolean = false
  def getMetaData: String = null
}


trait GenericNestedCodegen extends GenericCodegen {
  val IR: Expressions with Effects
  import IR._

  var shallow = false

  var scope: List[TP[_]] = Nil

  override def emitBlock(start: Exp[_])(implicit stream: PrintWriter): Unit = {
    // try to push stuff as far down into more control-dependent parts as
    // possible. this is the right thing to do for conditionals, but
    // for loops we'll need to do it the other way round (hoist stuff out). 
    // for lambda expressions there is no clear general strategy.

    val e1 = buildScheduleForResult(start) // deep list of deps
    shallow = true
    val e2 = buildScheduleForResult(start) // shallow list of deps (exclude stuff only needed by nested blocks)
    shallow = false

    //println("==== deep")
    //e1.foreach(println)
    //println("==== shallow")
    //e2.foreach(println)

    val e3 = e1.filter(e2 contains _) // shallow, but with the ordering of deep!!

    val e4 = e3.filterNot(scope contains _) // remove stuff already emitted

    val save = scope
    scope = e4 ::: scope

    for (TP(sym, rhs) <- e4) {
      emitNode(sym, rhs)      
    }

    start match {
      case Def(Reify(x, effects0)) =>
        // with the current implementation the code below is not
        // really necessary. all effects should have been emitted
        // because of the Reflect dependencies. it's still a good
        // sanity check though

        val effects = effects0.map { case s: Sym[a] => findDefinition(s).get }
        val actual = e4.filter(effects contains _)

        // actual must be a prefix of effects!
        assert(effects.take(actual.length) == actual, 
            "violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)

        val e5 = effects.drop(actual.length)

        for (TP(_, rhs) <- e5) {
          emitNode(Sym(-1), rhs)
        }
      case _ =>
    }

    scope = save
  }


  override def getBlockResult[A](s: Exp[A]): Exp[A] = s match {
    case Def(Reify(x, _)) => x
    case _ => super.getBlockResult(s)
  }
  

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Reflect(s, effects) =>
      emitNode(sym, s)
    case Reify(s, effects) =>
      // just ignore -- effects are accounted for in emitBlock
    case _ => super.emitNode(sym, rhs)
  }

  override def getFreeVarBlock(start: Exp[_], local: List[Sym[_]]): List[Sym[_]] = {
    // Do the same things as emitBlock would
    val e1 = buildScheduleForResult(start) // deep list of deps
    shallow = true
    val e2 = buildScheduleForResult(start) // shallow list of deps (exclude stuff only needed by nested blocks)
    shallow = false
    val e3 = e1.filter(e2 contains _) // shallow, but with the ordering of deep!!
    val e4 = e3.filterNot(scope contains _) // remove stuff already emitted
    val save = scope
    scope = e4 ::: scope

    // Find local symbols (including those passed as arguments to this method)
    var localList:List[Sym[_]] = e4.map(_.sym) ::: local.toList

    // Find free variables by subtracting local list from used list (shallow should be turned on)
    shallow = true
    var freeList:List[Sym[_]]  = e4.flatMap(syms).filter(!localList.contains(_))
    shallow = false

    // Iterate nodes to find out free variables in the nested blocks
    for (TP(sym, rhs) <- e4) {
      freeList = (getFreeVarNode(rhs) ::: freeList).filter(!localList.contains(_))
    }

    // restore scope
    scope = save

    // return free symbol list
    freeList.distinct
  }

  //override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = { Nil }
  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case Reflect(s, effects) => getFreeVarNode(s)
    case _ => Nil
  }

  def getEffectsBlock(start: Exp[_]): List[Sym[_]] = {
    val save = shallow
    shallow = false
    val stDeep = dep(start)

    // deep list of deps
    val e1 = GraphUtil.stronglyConnectedComponents[TP[_]](stDeep.flatMap(e => findDefinition(e).toList), { d =>
      dep(d.rhs).flatMap { e =>
        findDefinition(e).toList
      }
    }).flatten.reverse

    // deep on everything except start
    shallow = true
    val stShallow = dep(start)
    shallow = false

    val e2 = GraphUtil.stronglyConnectedComponents[TP[_]](stShallow.flatMap(e => findDefinition(e).toList), { d =>
      dep(d.rhs).flatMap { e =>
        findDefinition(e).toList
      }
    }).flatten.reverse

    // only the deep dependencies of start
    val e3 = e1 filterNot { e2 contains _ }

    val e4 = e3 flatMap { e =>
      e.sym match {
        case Def(Reflect(x, effects)) => List(e.sym): List[Sym[_]]
        case _ => Nil
      }
    }

    shallow = save
    e4
  }

  def reset {
    scope = Nil
    shallow = false
    IR.reset
  }

}