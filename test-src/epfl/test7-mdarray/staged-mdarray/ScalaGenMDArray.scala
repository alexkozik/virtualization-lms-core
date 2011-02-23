package scala.virtualization.lms
package epfl
package test7

import common._
import internal._
import original.MDArray
import java.io.PrintWriter
import collection.immutable.HashMap

trait BaseGenMDArray extends GenericNestedCodegen {
  val IR: MDArrayBaseExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case WithNode(iv, expr) if shallow => syms(iv) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case WithNode(iv, expr) => effectSyms(expr)
    case _ => super.boundSyms(e)
  }

  override def getFreeVarNode(rhs: Def[Any]): List[Sym[Any]] = rhs match {
    case WithNode(iv, expr) => getFreeVarBlock(iv,Nil) ::: getFreeVarBlock(expr,Nil)
    case _ => super.getFreeVarNode(rhs)
  }
}




trait TypedGenMDArray extends BaseGenMDArray with MDArrayTypingUnifier {

  import IR.{Exp, Sym, Def}

  var shapes: Map[Int, TypingVariable] = new HashMap()
  var values: Map[Int, TypingVariable] = new HashMap()
  var runtimeChecks: Map[Int, List[(TypingConstraint, TypingConstraint)]] = new HashMap()

  def performTyping(expr: Exp[Any]): Unit = {
    val result = doTyping(expr)
    shapes = result._1
    values = result._2
    runtimeChecks = result._3
  }

  def emitRuntimeChecks(expr: Sym[_], debug: Boolean = false)(implicit stream: PrintWriter): Unit = {
    /*
     * The logic here: as we generate runtime checks, we solve them and add the result to the other
     * runtime checks, possibly eliminating them :)
     */
    // TODO: Implement the code generation for constraints
    // TODO: Problem here: following the pureSubstitution, we have variables that don't necessarily respect scheduling
    runtimeChecks.contains(expr.id) match {
      case true =>
        for (runtimeCheck <- runtimeChecks(expr.id)) {
          // 0. Unpack pair
          val originalConstraint = runtimeCheck._1
          val pureSubstConstraint = runtimeCheck._2
          // 1. Solve the runtime check
          val unifyResult = unifyConstraint(pureSubstConstraint)
          // 2. Do the correct operation
          unifyResult match {
            case (false, _) => // constraint could not be solved
              stream.println("// " + quote(expr) + ": check    " + originalConstraint.toString + " [ not solvable ]")
            case (true, Nil) => // constraint solved with no substitution => already checked previously
              debug match {
                case true => stream.println("// " + quote(expr) + ": identity " + originalConstraint.toString + " [ solved previously ]")
                case _ => ;
              }
            case (true, substs) => // constraint solved with substitutions => check + apply substitutions
              stream.println("// " + quote(expr) + ": check    " + originalConstraint.toString + " [ solved ]")
              val substList = new SubstitutionList(substs)
              runtimeChecks = runtimeChecks.map {
                case (index, list) => (index, list.map {case (orig, modif) => (orig, substList(modif))})
              }
          }
        }
      case false =>
        debug match {
          case true =>
            stream.println("// " + quote(expr) + ": no runtime checks")
          case false =>
            ;
        }
    }
  }

  def getShapeLength(sym: Sym[_]) = getLength(shapes(sym.id))
  def getValueLength(sym: Sym[_]) = getLength(values(sym.id))
  def getShapeValue(sym: Sym[_]) = getValue(shapes(sym.id))
  def getValueValue(sym: Sym[_]) = getValue(values(sym.id))

  def emitShapeValue(sym: Sym[_])(implicit stream: PrintWriter): Unit =
    stream.println("// " + quote(sym) + ": shape=" + shapes(sym.id) + "    value=" + values(sym.id))
}




// TODO: Why are code generators specific? Couldn't we write "ScalaGenMDArray { this: ScalaGenFat => ..." ?
trait ScalaGenMDArray extends ScalaGenEffect with TypedGenMDArray {

  override val IR: MDArrayBaseExp
  import IR._

  def emitSymDecl(sym: Sym[Any], stripped: Boolean = false, debug: Boolean = false)(implicit stream: PrintWriter) = {

    // emit the debug info: shape, value and others
    debug match {
      case true =>
        stream.println()
        emitShapeValue(sym)
      case _ =>
        ;
    }

    // emit the runtime checks
    emitRuntimeChecks(sym, debug)

    // emit the definition
    stream.print("val " + quote(sym) + ": " + getType(sym, stripped) + " = ")
  }

  def getType(sym: Sym[_], stripped: Boolean = false)(implicit stream: PrintWriter): String = stripped match {
    case false => sym.typeManifest.toString.replace("scala.virtualization.lms.epfl.test7.original.MDArray", "MDArray")
    case true => stripMDArray(sym.typeManifest).get // Let it crash at runtime if it's not a MDArray
  }

  def emitOperationPrologue(sym: Sym[Any], exp: Exp[Any])(implicit stream: PrintWriter) = {
    emitSymDecl(sym);
    stream.println("{")
    stream.println("val result = new Array[" + stripMDArray(sym.typeManifest).get + "](shape(" + quote(exp) + ").content().foldLeft(1)((a,b) => a*b))")
    stream.println("for(i <- List.range(0, result.length))")
  }

  def emitOperationEpilogue(sym: Sym[Any], exp: Exp[Any], opName: String)(implicit stream: PrintWriter) = {
    stream.println("internalReshape(shape(" + quote(exp) + "), result, \"" + opName + "\")")
    stream.println("}")
  }

  // This makes it easy to get the elements we need
  def findAndCast[T](e: Any): Option[T] = e match {
    case s: Sym[_] => findAndCast[T](findDefinition(s).get.rhs)
    case d: Def[_] => Some(d.asInstanceOf[T]) // hopefully it's the correct type
    case _ => None
  }

  def emitGenArray[A](sym: Sym[_], wl: List[Exp[MDArray[A]]], shp: Exp[MDArray[Int]])(implicit stream: PrintWriter) = {

    val shpSym: Sym[_] = shp.asInstanceOf[Sym[_]]

    def emitGenArrayAction(iv: String, loopResult: String) = {
      stream.println("if (result == null) {")
      stream.println("// create the array and shape")
      stream.println("result = new Array[" + stripMDArray(sym.typeManifest).get + "](" + quote(shp) + ".content().foldLeft(1)((a,b) => a*b) * " + loopResult + ".content().length)")
      stream.println("rshape = shape(" + loopResult + ").content()")
      stream.println("} else {")
      stream.println("// check shape -- this WILL be redundant due to runtime checks")
      stream.println("if (shape(" + loopResult + ").content().toList != rshape.toList) throw new Exception(opName + \": Incompatible shapes:\" + rshape.toList.toString + \" vs \" + shape(" + loopResult + ").content().toList.toString)")
      stream.println("}")
      stream.println("// copy new content")
      stream.println("val mainIndex: Int = flatten(" + quote(shpSym) + " ::: rshape.toList, " + iv + " ::: zeros(rshape.length), opName)")
      stream.println("for (innerIndex <- List.range(0, rshape.length)) {")
      stream.println("result(mainIndex + innerIndex) = " + loopResult + "(innerIndex)")
      stream.println("}")
    }

    emitSymDecl(sym);
    stream.println("{")
    stream.println("val opName: String = \"genarray\"")
    stream.println("var result: Array[" + stripMDArray(sym.typeManifest).get + "] = null")
    stream.println("var rshape: Array[Int] = null")
    for (withNode <- wl)
      // Let this fail quickly if it's not a WithNode
      emitWithLoopModifier(withNode.asInstanceOf[Sym[_]], findAndCast[WithNode[_]](withNode).get, emitGenArrayAction)

    stream.println("internalReshape(" + quote(shpSym) + " ::: rshape.toList, result, opName)")
    stream.println("}")
  }

  def emitModArray[A](sym: Sym[_], wl: List[Exp[MDArray[A]]], array: Exp[MDArray[A]])(implicit stream: PrintWriter) = {

    val arraySym: Sym[_] = array.asInstanceOf[Sym[_]]

    def emitModArrayAction(iv: String, loopResult: String) = {
      stream.println("if (rshape == null) {")
      stream.println("rshape = shape(" + quote(arraySym) + ").drop(" + iv + ".content().length)")
      stream.println("}")
      stream.println("val mainIndex: Int = flatten(shape(" + quote(arraySym) + "), " + iv + " ::: zeros(dim(" + quote(arraySym) + ") - " + iv + ".content().length), opName)")
      stream.println("// check shape -- this WILL be redundant due to runtime checks")
      stream.println("if (shape(" + loopResult + ").content().toList != rshape) throw new Exception(opName + \": Incompatible shapes:\" + rshape.toList.toString + \" vs \" + shape(" + loopResult + ").content().toList.toString)")
      stream.println("// copy new content")
      stream.println("for (innerIndex <- List.range(0, " + loopResult + ".content().length)) {")
      stream.println("result(mainIndex + innerIndex) = " + loopResult + ".content()(innerIndex)")
      stream.println("}")
    }

    emitSymDecl(sym);
    stream.println("{")
    stream.println("val opName: String = \"modarray\"")
    stream.println("var result: Array[" + stripMDArray(sym.typeManifest).get + "] = new Array[" + stripMDArray(sym.typeManifest).get + "](shape(" + quote(arraySym) + ").content().foldLeft(1)((a,b) => a*b))")
    stream.println("for (i <- List.range(0, result.length)) {")
    stream.println("result(i) = " + quote(arraySym) + ".content()(i)")
    stream.println("}")
    stream.println("var rshape: List[Int] = null")

    for (withNode <- wl)
      // Let this fail quickly if it's not a WithNode
      emitWithLoopModifier(withNode.asInstanceOf[Sym[_]], findAndCast[WithNode[_]](withNode).get, emitModArrayAction)

    stream.println("internalReshape(shape(" + quote(arraySym) + ") ::: rshape.toList, result, opName)")
    stream.println("}")
  }


  def emitWithLoopModifier(withNodeSym: Sym[_], withLoop: WithNode[_], emitAction: (String, String) => Unit)(implicit stream: PrintWriter) = {
    // emit existing constraints
    stream.println("// with: " + withLoop.toString)
    emitRuntimeChecks(withNodeSym)
    stream.println("// index vector")
    emitRuntimeChecks(withLoop.iv.asInstanceOf[Sym[_]])

    val iv: IndexVector = findAndCast[IndexVector](withLoop.iv).get // fail fast
    val ivSym: Sym[_] = findDefinition(iv).get.sym

    // emit actual with loop
    getValueLength(ivSym) match {
      case Some(l) =>
        // emit loop
        for (index <- List.range(0, l)) {
          stream.println("val lb" + index + ": Int = " + quote(iv.lb) + ".content()(" + index + ")")
          stream.println("val ub" + index + ": Int = " + quote(iv.ub) + ".content()(" + index + ")")
          stream.println("val step" + index + ": Int = " + quote(iv.step) + ".content()(" + index + ")")
          stream.println("val width" + index + ": Int = " + quote(iv.width) + ".content()(" + index + ")")
          stream.println("val ll" + index + ": Int = if (" + quote(iv.lbStrict) + ") lb" + index + " + 1 else lb" + index + "")
          stream.println("val ul" + index + ": Int = if (" + quote(iv.ubStrict) + ") ub" + index + " else ub" + index + " + 1")
          stream.println("for (iv" + index + " <- List.range(ll" + index + ", ul" + index + ")) {")
          stream.println("if ((iv" + index + " - lb" + index + ") % step" + index + " <= width" + index + ") {")
        }
        // emit loop content
        stream.print("val " + quote(withLoop.iv) + ": " + getType(ivSym) + " = ")
        stream.println(List.range(0, l).map(i => "iv" + i).mkString("", "::","::Nil"))
        stream.println("val iv: " + getType(ivSym) + " = " + quote(withLoop.iv))
        stream.println("val feval: " + getType(withNodeSym) + " = {")
        emitBlock(withLoop.expr)
        stream.println(quote(getBlockResult(withLoop.expr)))
        stream.println("}")
        // emit loop action
        stream.println("// the action of this loop:")
        emitAction("iv", "feval")
        // emit loop end
        for (index <- List.range(0, l)) {
          stream.println("} // if ((iv" + index + " ...")
          stream.println("} // for (iv" + index + " ...")
        }
      case _ =>
        // emit loop
        stream.println("for (iv <- iterateWithStep(_lb=" + quote(iv.lb) + ", lbStrict=" + quote(iv.lbStrict) + ", ubStrict=" + quote(iv.ubStrict) + ", _ub=" + quote(iv.ub) + ", step=" + quote(iv.step) + ", width=" + quote(iv.width) + ", opName=opName) {")
        // emit loop content
        stream.println("val " + quote(withLoop.iv) + ": " + getType(ivSym) + " = iv")
        stream.println("val feval: " + getType(ivSym) + " = {")
        emitBlock(withLoop.expr)
        stream.println(quote(getBlockResult(withLoop.expr)))
        stream.println("}")
        // emit loop action
        stream.println("// the action of this loop:")
        emitAction("iv", "feval")
        // emit loop end
        stream.println("}")
    }
  }

  def emitWithLoop[A](s: Sym[_], wl: Exp[MDArray[A]], suffix: String)(implicit stream: PrintWriter) = {
    emitSymDecl(s)

    findAndCast[WithNode[_]](wl) match {
      case Some(wloop) => findAndCast[IndexVector](wloop.iv) match {
        case Some(iv) =>
          // generate code with for loops
          getShapeLength(wloop.iv.asInstanceOf[Sym[_]]) match {
            case Some(l) => stream.println("// transform into nested for loop")
            case _ => stream.println("// keep as with loop")
          }
          stream.println("With(lb=" + quote(iv.lb) + ", lbStrict=" + quote(iv.lbStrict) + ", ubStrict=" + quote(iv.ubStrict) + ", ub=" + quote(iv.ub) + ", step=" + quote(iv.step) + ", width=" + quote(iv.width) + ", function = ")
          stream.println(quote(wloop.iv) + " => {")
          emitBlock(wloop.expr)
          stream.println(quote(getBlockResult(wloop.expr)))
          stream.print("})" + suffix)
        case None => stream.print("null // Index vector not found!")
      }
      case None => stream.print("null // With loop not found!")
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    case kc: KnownAtCompileTime[_] =>
      emitSymDecl(sym)
      stream.println("internalReshape(" + (kc.value.shape.content.map(t => t.toString).toList ::: ("Nil"::Nil)).mkString("::") + ", Array(" + kc.value.content.mkString(", ") +"), \"knownAtCompileTime\")")
    case kr: KnownAtRuntime[_] =>
      emitSymDecl(sym)
      stream.println(kr.name + " // this is a function argument")
    case fl: FromList[_] =>
      emitSymDecl(sym)
      stream.println("internalReshape(" + quote(fl.value) + ".length::Nil, " + quote(fl.value) + ", \"fromList\")")
    case fa: FromArray[_] =>
      emitSymDecl(sym)
      stream.println("internalReshape(" + quote(fa.value) + ".length::Nil, " + quote(fa.value) + ", \"fromArray\")")
    case fv: FromValue[_] =>
      emitSymDecl(sym, stripped = true)
      stream.println(quote(fv.value))
    case tl: ToList[_] =>
      emitSymDecl(sym)
      stream.println(quote(tl.value) + ".content().toList // toList")
    case ta: ToArray[_] =>
      emitSymDecl(sym)
      stream.println(quote(ta.value) + ".content() // toArray")
    case tv: ToValue[_] =>
      // This will automatically unbox in case there's boxing done...
      emitSymDecl(sym)
      stream.println(quote(tv.value))
    case td: ToDim[_] =>
      emitSymDecl(sym)
      stream.println("dim(" + quote(td.a) + ")")
    case ts: ToShape[_] =>
      emitSymDecl(sym)
      stream.println("shape(" + quote(ts.a) + ")")
    case rs: Reshape[_] =>
      emitSymDecl(sym)
      stream.println("reshape(" + quote(rs.shp) + ", " + quote(rs.a) + ")")
    case sel: Sel[_] =>
      // Get rid of unnecessary boxing
      getShapeLength(sym) match {
        case Some(0) =>
          emitSymDecl(sym, true)
          stream.println(quote(sel.a) + ".content()(flatten(shape(" + quote(sel.a) + "), " + quote(sel.iv) + ", \"sel\"))")
        case _ =>
          emitSymDecl(sym)
          stream.println("sel(" + quote(sel.iv) + ", " + quote(sel.a) + ")")
      }
    case cat: Cat[_] =>
      emitSymDecl(sym)
      stream.println("cat(" + quote(cat.d) + ", " + quote(cat.a) + ", " + quote(cat.b) + ")")
    case red: Reduce[_, _] =>
      emitSymDecl(sym)
      stream.println(quote(red.a) + ".content().foldLeft(" + quote(red.z) + ")(" + red.opRepr + ")  // reduce: " + red.opName)
    case in: InfixOpAA[_, _] =>
      emitOperationPrologue(sym, in.array1)
      stream.println("result(i) = " + quote(in.array1) + ".content()(i) " + in.opName + "  " + quote(in.array2) + ".content()(i)")
      emitOperationEpilogue(sym, in.array1, "infixOpAA")
    case in: InfixOpAE[_, _] =>
      emitOperationPrologue(sym, in.array)
      stream.println("result(i) = " + quote(in.array) + ".content()(i) " + in.opName + "  " + quote(in.element))
      emitOperationEpilogue(sym, in.array, "infixOpAE")
    case un: UnaryOp[_, _] =>
      emitOperationPrologue(sym, un.array)
      stream.println("result(i) = " + un.opName + quote(un.array) + ".content()(i)")
      emitOperationEpilogue(sym, un.array, "unaryOp")
    case wh: Where[_] =>
      emitOperationPrologue(sym, wh.array1)
      stream.println("result(i) = if (" + quote(wh.cond) + ".content()(i)) " + quote(wh.array1) + ".content()(i) else " + quote(wh.array2) + ".content()(i)")
      emitOperationEpilogue(sym, wh.array1, "where")
    case va: Values[_] =>
      emitSymDecl(sym); stream.println("{")
      stream.println("val result = new Array[Int](" + quote(va.dim) + ")")
      stream.println("for(i <- List.range(0, result.length))")
      stream.println("result(i) = " + quote(va.value))
      stream.println("internalReshape(" + quote(va.dim) + "::Nil, result, \"values\")")
      stream.println("}")
    case in: IndexVector =>
      ; // nothing at this point
    case wn: WithNode[_] =>
      ; // nothing at this point
    case ga: GenArrayWith[_] =>
      stream.println
      emitGenArray(sym, ga.lExpr, ga.shp)
      stream.println
    case ma: ModArrayWith[_] =>
      stream.println
      emitModArray(sym, ma.lExpr, ma.a)
      stream.println
    case fa: FoldArrayWith[_] =>
      stream.println
      // TODO: Generate code for any fold function
      // TODO: With loop specialization in fold :)
      emitWithLoop(sym, fa.wExpr, ".Fold((a, b) => a + b, " + quote(fa.neutral) + ")")
      stream.println
    case eq: IntPlus => emitSymDecl(sym); stream.println(quote(eq.a) + " + " + quote(eq.b))
    case eq: IntMinus => emitSymDecl(sym); stream.println(quote(eq.a) + " - " + quote(eq.b))
    case eq: IntEqual => emitSymDecl(sym); stream.println(quote(eq.a) + " == " + quote(eq.b))
    case eq: IntLess => emitSymDecl(sym); stream.println(quote(eq.a) + " < " + quote(eq.b))
    case x => super.emitNode(sym, rhs) + " // dunno how to generate"

  }

  // TODO: Convert this back to if, but if is overridden now, so we can't use it
  // TODO: Switch back to java.lang.Class comparison, string comparison is so middle ages :)
  def stripMDArray(typeManifest: Manifest[_]): Option[String] =
    ((typeManifest.erasure == classOf[MDArray[_]]) && (typeManifest.typeArguments.length == 1)) match {
    case true => Some(typeManifest.typeArguments.head.toString)
    case false => None
  }

  // the emitSource in ScalaCodeGen is not exactly what we need - we need to select the parameters on my own
  def emitSource(expr: Exp[Any], className: String)(implicit stream: PrintWriter): Unit = {

    // Do typing!
    performTyping(expr)

    // We need to build the AST and obtain the input values
    val allNodes: List[TP[Any]] = buildScheduleForResult(expr)
    val inputNodes: List[TP[Any]] = allNodes.filter(tp => tp.rhs.isInstanceOf[KnownAtRuntime[_]])
    val inputNodeData: List[(String, String)] = inputNodes.map(tp => {
      val node: KnownAtRuntime[_] = tp.rhs.asInstanceOf[KnownAtRuntime[_]]
      val name: String = node.name
      val stype: String = tp.sym.typeManifest.toString
      (name, stype)
    })

    val inputTypes = inputNodeData.map((p: (String, String)) => p._2).mkString(", ")
    val inputVars  = inputNodeData.map((p: (String, String)) => p._1 + ": " + p._2).mkString(", ")
    val outputSym: Sym[Any] = expr match {
      case s: Sym[Any] => s
      case d: Def[Any] => findOrCreateDefinition(d).sym
    }
    val outputType = outputSym.typeManifest.toString

    stream.println("/*****************************************\n"+
                   "  Emitting Generated Code                  \n"+
                   "*******************************************/\n")


    // TODO: Unhardcode this
    stream.println("package scala.virtualization.lms")
    stream.println("package epfl")
    stream.println("package test7")
    stream.println("package original")
    stream.println()
    stream.println("import test7.original.Conversions._")
    stream.println("import test7.original.Operations._")
    stream.println("import test7.original.SpecificOperations._")
    stream.println()
    stream.println("class "+className+" extends (("+inputTypes+")=>("+outputType+")) {\n\n")
    stream.println("def apply("+inputVars+"): "+outputType+" = {\n")

    emitBlock(expr)(stream)
    stream.println(quote(getBlockResult(expr)))

    stream.println("}")
    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of Generated Code                  \n"+
                   "*******************************************/")
  }
}