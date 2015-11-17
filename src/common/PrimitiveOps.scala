package scala.lms
package common

import scala.lms.internal.GenerationFailedException
import scala.lms.util.OverloadHack
import scala.reflect.SourceContext

trait LiftPrimitives {
  this: PrimitiveOps =>

  implicit def intToRepInt(x: Int) = unit(x)  
  implicit def floatToRepFloat(x: Float) = unit(x)
  implicit def doubleToRepDouble(x: Double) = unit(x)
  
  // precision-widening promotions
  implicit def chainIntToRepFloat[A:Manifest](x: A)(implicit c: A => Rep[Int]): Rep[Float] = repIntToRepFloat(c(x))
  implicit def chainFloatToRepDouble[A:Manifest](x: A)(implicit c: A => Rep[Float]): Rep[Double] = repFloatToRepDouble(c(x))
}



/**
 * This file is extremely boilerplate and redundant and does not take advantage of any of
 * Scala's type hierarchy to reduce the amount of IR nodes or code generation require.
 * It is in semi-desperate need of a refactor.
 */
trait PrimitiveOps extends Variables with OverloadHack { 
  this: ImplicitOps =>

  /**
   * Primitive conversions
   */
  implicit def repIntToRepDouble(x: Rep[Int]): Rep[Double] = x.toDouble
  implicit def repIntToRepFloat(x: Rep[Int]): Rep[Float] = x.toFloat
  implicit def repFloatToRepDouble(x: Rep[Float]): Rep[Double] = x.toDouble
      
  
  /**
   * Enumerate all combinations of primitive math.
   * Avoids certain fragile behavior, including compiler crashes and some erroneous or inaccessible type errors.
   */  
  def infix_-(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_minus(unit(lhs), rhs)
  def infix_-(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(unit(lhs), rhs)
  def infix_-(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_minus(unit(lhs),rhs)  
  def infix_-(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(unit(lhs), rhs)
  def infix_-(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs), rhs)
  def infix_-(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_minus(unit(lhs),rhs)  
  def infix_-(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs),rhs)
  def infix_-(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_minus(unit(lhs),rhs)
  def infix_-(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_minus(lhs, unit(rhs))  
  def infix_-(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_minus(lhs, unit(rhs))
  def infix_-(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = int_minus(lhs, rhs)
  def infix_-(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_minus(repIntToRepFloat(lhs), rhs)
  def infix_-(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_minus(repIntToRepDouble(lhs), rhs)
  def infix_-(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded4, ctx: SourceContext): Rep[Float] = float_minus(lhs,repIntToRepFloat(rhs))  
  def infix_-(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = float_minus(lhs, rhs)
  def infix_-(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_minus(repFloatToRepDouble(lhs), rhs)
  def infix_-(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = double_minus(lhs,repIntToRepDouble(rhs))  
  def infix_-(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded8, ctx: SourceContext): Rep[Double] = double_minus(lhs,repFloatToRepDouble(rhs))
  def infix_-(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = double_minus(lhs,rhs)    

  def infix_+(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_plus(unit(lhs), rhs)
  def infix_+(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(unit(lhs), rhs)
  def infix_+(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_plus(unit(lhs),rhs)  
  def infix_+(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(unit(lhs), rhs)
  def infix_+(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs), rhs)
  def infix_+(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_plus(unit(lhs),rhs)  
  def infix_+(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs),rhs)
  def infix_+(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_plus(unit(lhs),rhs)
  def infix_+(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_plus(lhs, unit(rhs))  
  def infix_+(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_plus(lhs, unit(rhs))
  def infix_+(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded15, ctx: SourceContext): Rep[Int] = int_plus(lhs, rhs)
  def infix_+(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded16, ctx: SourceContext): Rep[Float] = float_plus(repIntToRepFloat(lhs), rhs)
  def infix_+(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded17, ctx: SourceContext): Rep[Double] = double_plus(repIntToRepDouble(lhs), rhs)
  def infix_+(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded18, ctx: SourceContext): Rep[Float] = float_plus(lhs,repIntToRepFloat(rhs))  
  def infix_+(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded19, ctx: SourceContext): Rep[Float] = float_plus(lhs, rhs)
  def infix_+(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded20, ctx: SourceContext): Rep[Double] = double_plus(repFloatToRepDouble(lhs), rhs)
  def infix_+(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded21, ctx: SourceContext): Rep[Double] = double_plus(lhs,repIntToRepDouble(rhs))  
  def infix_+(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded22, ctx: SourceContext): Rep[Double] = double_plus(lhs,repFloatToRepDouble(rhs))
  def infix_+(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded23, ctx: SourceContext): Rep[Double] = double_plus(lhs,rhs)    

  def infix_*(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_times(unit(lhs), rhs)
  def infix_*(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(unit(lhs), rhs)
  def infix_*(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_times(unit(lhs),rhs)  
  def infix_*(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(unit(lhs), rhs)
  def infix_*(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(unit(lhs), rhs)
  def infix_*(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_times(unit(lhs),rhs)  
  def infix_*(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_times(unit(lhs),rhs)
  def infix_*(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_times(unit(lhs),rhs)
  def infix_*(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_times(lhs, unit(rhs))  
  def infix_*(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_times(lhs, unit(rhs))
  def infix_*(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = int_times(lhs, rhs)
  def infix_*(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_times(repIntToRepFloat(lhs), rhs)
  def infix_*(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_times(repIntToRepDouble(lhs), rhs)
  def infix_*(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded4, ctx: SourceContext): Rep[Float] = float_times(lhs,repIntToRepFloat(rhs))  
  def infix_*(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = float_times(lhs, rhs)
  def infix_*(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_times(repFloatToRepDouble(lhs), rhs)
  def infix_*(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = double_times(lhs,repIntToRepDouble(rhs))  
  def infix_*(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded8, ctx: SourceContext): Rep[Double] = double_times(lhs,repFloatToRepDouble(rhs))
  def infix_*(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = double_times(lhs,rhs)    

  def infix_/(lhs: Int, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = int_divide(unit(lhs), rhs)
  def infix_/(lhs: Int, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(unit(lhs), rhs)
  def infix_/(lhs: Int, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Float] = float_divide(unit(lhs),rhs)  
  def infix_/(lhs: Float, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(unit(lhs), rhs)
  def infix_/(lhs: Float, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs), rhs)
  def infix_/(lhs: Double, rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Double] = double_divide(unit(lhs),rhs)  
  def infix_/(lhs: Double, rhs: Rep[Float])(implicit o: Overloaded1, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs),rhs)
  def infix_/(lhs: Double, rhs: Rep[Double])(implicit o: Overloaded2, ctx: SourceContext): Rep[Double] = double_divide(unit(lhs),rhs)
  def infix_/(lhs: Rep[Int], rhs: Int)(implicit ctx: SourceContext): Rep[Int] = int_divide(lhs, unit(rhs))  
  def infix_/(lhs: Rep[Int], rhs: Double)(implicit ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Int], rhs: Float)(implicit ctx: SourceContext): Rep[Float] = float_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Int)(implicit o: Overloaded1, ctx: SourceContext): Rep[Float] = float_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Float)(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Float], rhs: Double)(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Int)(implicit o: Overloaded4, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Float)(implicit o: Overloaded5, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Double], rhs: Double)(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_divide(lhs, unit(rhs))
  def infix_/(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, ctx: SourceContext): Rep[Int] = int_divide(lhs, rhs)
  def infix_/(lhs: Rep[Int], rhs: Rep[Float])(implicit o: Overloaded2, ctx: SourceContext): Rep[Float] = float_divide(repIntToRepFloat(lhs), rhs)
  def infix_/(lhs: Rep[Int], rhs: Rep[Double])(implicit o: Overloaded3, ctx: SourceContext): Rep[Double] = double_divide(repIntToRepDouble(lhs), rhs)
  def infix_/(lhs: Rep[Float], rhs: Rep[Int])(implicit o: Overloaded4, ctx: SourceContext): Rep[Float] = float_divide(lhs,repIntToRepFloat(rhs))  
  def infix_/(lhs: Rep[Float], rhs: Rep[Float])(implicit o: Overloaded5, ctx: SourceContext): Rep[Float] = float_divide(lhs, rhs)
  def infix_/(lhs: Rep[Float], rhs: Rep[Double])(implicit o: Overloaded6, ctx: SourceContext): Rep[Double] = double_divide(repFloatToRepDouble(lhs), rhs)
  def infix_/(lhs: Rep[Double], rhs: Rep[Int])(implicit o: Overloaded7, ctx: SourceContext): Rep[Double] = double_divide(lhs,repIntToRepDouble(rhs))  
  def infix_/(lhs: Rep[Double], rhs: Rep[Float])(implicit o: Overloaded8, ctx: SourceContext): Rep[Double] = double_divide(lhs,repFloatToRepDouble(rhs))
  def infix_/(lhs: Rep[Double], rhs: Rep[Double])(implicit o: Overloaded9, ctx: SourceContext): Rep[Double] = double_divide(lhs,rhs)      

  /**
   *  Double
   */
  implicit def doubleToDoubleOps(n: Double): DoubleOpsCls = new DoubleOpsCls(unit(n))
  implicit def repDoubleToDoubleOps(n: Rep[Double]): DoubleOpsCls = new DoubleOpsCls(n)
  implicit def varDoubleToDoubleOps(n: Var[Double]): DoubleOpsCls = new DoubleOpsCls(readVar(n))
  
  object Double {
    def parseDouble(s: Rep[String])(implicit pos: SourceContext) = obj_double_parse_double(s)
    def PositiveInfinity(implicit pos: SourceContext) = obj_double_positive_infinity
    def NegativeInfinity(implicit pos: SourceContext) = obj_double_negative_infinity
    def MinValue(implicit pos: SourceContext) = obj_double_min_value
    def MaxValue(implicit pos: SourceContext) = obj_double_max_value
  }

  class DoubleOpsCls(lhs: Rep[Double]){
    def floatValue()(implicit pos: SourceContext) = double_to_float(lhs)
    def toInt(implicit pos: SourceContext) = double_to_int(lhs)
    def toFloat(implicit pos: SourceContext) = double_to_float(lhs)
  }

  def obj_double_parse_double(s: Rep[String])(implicit pos: SourceContext): Rep[Double]
  def obj_double_positive_infinity(implicit pos: SourceContext): Rep[Double]
  def obj_double_negative_infinity(implicit pos: SourceContext): Rep[Double]
  def obj_double_min_value(implicit pos: SourceContext): Rep[Double]
  def obj_double_max_value(implicit pos: SourceContext): Rep[Double]
  def double_plus(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_minus(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_times(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]
  def double_divide(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double]  
  def double_to_int(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Int]
  def double_to_float(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Float]
  
  /**
   * Float
   */
  object Float {
    def parseFloat(s: Rep[String])(implicit pos: SourceContext) = obj_float_parse_float(s)
  }

  def infix_toInt(lhs: Rep[Float])(implicit o: Overloaded1, pos: SourceContext): Rep[Int] = float_to_int(lhs)
  def infix_toDouble(lhs: Rep[Float])(implicit o: Overloaded1, pos: SourceContext): Rep[Double] = float_to_double(lhs) 

  def obj_float_parse_float(s: Rep[String])(implicit pos: SourceContext): Rep[Float]
  def float_plus(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_minus(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_times(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]
  def float_divide(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float]         
  def float_to_int(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Int]
  def float_to_double(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Double]
  
  /**
   * Int
   */
  object Integer {
    def parseInt(s: Rep[String])(implicit pos: SourceContext) = obj_integer_parse_int(s)
  }

  object Int {
    def MaxValue(implicit pos: SourceContext) = obj_int_max_value
    def MinValue(implicit pos: SourceContext) = obj_int_min_value
  }

  def infix_toFloat(lhs: Rep[Int])(implicit o: Overloaded2, pos: SourceContext): Rep[Float] = int_to_float(lhs)
  def infix_toDouble(lhs: Rep[Int])(implicit o: Overloaded2, pos: SourceContext): Rep[Double] = int_to_double(lhs) 

  implicit def intToIntOps(n: Int): IntOpsCls = new IntOpsCls(unit(n))
  implicit def repIntToIntOps(n: Rep[Int]): IntOpsCls = new IntOpsCls(n)
  implicit def varIntToIntOps(n: Var[Int]): IntOpsCls = new IntOpsCls(readVar(n))
    
  class IntOpsCls(lhs: Rep[Int]){
    // TODO (tiark): either of these cause scalac to crash        
    //def /[A](rhs: Rep[A])(implicit mA: Manifest[A], f: Fractional[A], o: Overloaded1) = int_divide_frac(lhs, rhs)
    //def /(rhs: Rep[Int]) = int_divide(lhs, rhs)
    // TODO Something is wrong if we just use floatValue. implicits get confused
    def floatValueL()(implicit pos: SourceContext) = int_to_float(lhs)
    def doubleValue()(implicit pos: SourceContext) = int_to_double(lhs)
    def unary_~()(implicit pos: SourceContext) = int_bitwise_not(lhs)
    def toLong(implicit pos: SourceContext) = int_to_long(lhs)
    def toDouble(implicit pos: SourceContext) = int_to_double(lhs)
    def toFloat(implicit pos: SourceContext) = int_to_float(lhs)        
  }

  
  def infix_%(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_mod(lhs, rhs)
  def infix_&(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_bitwise_and(lhs, rhs)
  def infix_|(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_bitwise_or(lhs, rhs)
  def infix_^(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_bitwise_xor(lhs, rhs)
  def infix_<<(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_left_shift(lhs, rhs)
  def infix_>>(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_right_shift_arithmetic(lhs, rhs)
  def infix_>>>(lhs: Rep[Int], rhs: Rep[Int])(implicit o: Overloaded1, pos: SourceContext) = int_right_shift_logical(lhs, rhs)

  def obj_integer_parse_int(s: Rep[String])(implicit pos: SourceContext): Rep[Int]
  def obj_int_max_value(implicit pos: SourceContext): Rep[Int]
  def obj_int_min_value(implicit pos: SourceContext): Rep[Int]
  def int_plus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_minus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_times(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  // def int_divide_frac[A:Manifest:Fractional](lhs: Rep[Int], rhs: Rep[A])(implicit pos: SourceContext): Rep[A]
  def int_divide(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  
  def int_mod(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_bitwise_or(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_bitwise_and(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_bitwise_xor(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_bitwise_not(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Int]
  def int_to_long(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Long]
  def int_to_float(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Float]
  def int_to_double(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Double]
  def int_left_shift(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_right_shift_arithmetic(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def int_right_shift_logical(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]

  /**
   * Long
   */
  object Long {
    def parseLong(s: Rep[String])(implicit pos: SourceContext) = obj_long_parse_long(s)
  }

  def infix_%(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_mod(lhs, rhs)
  def infix_&(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_bitwise_and(lhs, rhs)
  def infix_|(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_bitwise_or(lhs, rhs)
  def infix_<<(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded2, pos: SourceContext) = long_left_shift(lhs, rhs)
  def infix_>>>(lhs: Rep[Long], rhs: Rep[Int])(implicit o: Overloaded2, pos: SourceContext) = long_right_shift_arithmetic(lhs, rhs)
  def infix_toInt(lhs: Rep[Long])(implicit o: Overloaded2, pos: SourceContext) = long_to_int(lhs)
    
  def obj_long_parse_long(s: Rep[String])(implicit pos: SourceContext): Rep[Long]
  def long_mod(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_bitwise_and(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_bitwise_or(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long]
  def long_left_shift(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  def long_right_shift_arithmetic(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long]
  def long_to_int(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Int]
}

trait PrimitiveOpsExp extends PrimitiveOps with EffectExp {
  this: ImplicitOps =>

  // General primitive numeric operations
  // Manifests included directly in the class to simplify matching
  case class PrimParse[A](s: Exp[String], m: Manifest[A]) extends Def[A]
  case class PrimConvert[A, B](lhs: Exp[A], mA: Manifest[A], mB: Manifest[B], n: Numeric[A]) extends Def[B] {
    override def toString = s"PrimConvert($lhs,$mA,$mB)"
  }
  case class PrimPlus[A](lhs: Exp[A], rhs: Exp[A], m: Manifest[A], n: Numeric[A]) extends Def[A] {
    override def toString = s"PrimPlus($lhs,$rhs,$m)"
  }
  case class PrimMinus[A](lhs: Exp[A], rhs: Exp[A], m: Manifest[A], n: Numeric[A]) extends Def[A] {
    override def toString = s"PrimMinus($lhs,$rhs,$m)"
  }
  case class PrimTimes[A](lhs: Exp[A], rhs: Exp[A], m: Manifest[A], n: Numeric[A]) extends Def[A] {
    override def toString = s"PrimTimes($lhs,$rhs,$m)"
  }
  case class PrimMinValue[A](m: Manifest[A]) extends Def[A]
  case class PrimMaxValue[A](m: Manifest[A]) extends Def[A]

  // Primitive floating point operations
  case class PrimFPDivide[A](lhs: Exp[A], rhs: Exp[A], m: Manifest[A], f: Fractional[A]) extends Def[A] {
    override def toString = s"PrimFPDivide($lhs,$rhs,$m)"
  }
  case class PrimFPPositiveInfinity[A](m: Manifest[A]) extends Def[A]
  case class PrimFPNegativeInfinity[A](m: Manifest[A]) extends Def[A]

  // Primitive integral operations
  case class PrimIntegralDivide[A](lhs: Exp[A], rhs: Exp[A], m: Manifest[A], i: Integral[A]) extends Def[A] {
    override def toString = s"PrimIntegralDivide($lhs,$rhs,$m)"
  }
  case class PrimIntegralMod[A](lhs: Exp[A], rhs: Exp[A], m: Manifest[A], i: Integral[A]) extends Def[A] {
    override def toString = s"PrimIntegralMod($lhs,$rhs,$m)"
  }
  case class PrimIntegralBitwiseOr[A](lhs: Exp[A], rhs: Exp[A], m: Manifest[A]) extends Def[A]
  case class PrimIntegralBitwiseAnd[A](lhs: Exp[A], rhs: Exp[A], m: Manifest[A]) extends Def[A]
  case class PrimIntegralBitwiseXor[A](lhs: Exp[A], rhs: Exp[A], m: Manifest[A]) extends Def[A]
  case class PrimIntegralBitwiseNot[A](lhs: Exp[A], m: Manifest[A]) extends Def[A]
  case class PrimIntegralLeftShift[A](lhs: Exp[A], rhs: Exp[Int], m: Manifest[A]) extends Def[A]
  case class PrimIntegralRightShiftArith[A](lhs: Exp[A], rhs: Exp[Int], m: Manifest[A]) extends Def[A]
  case class PrimIntegralRightShiftLogical[A](lhs: Exp[A], rhs: Exp[Int], m: Manifest[A]) extends Def[A]

  def prim_parse[A](s: Exp[String])(implicit m: Manifest[A], pos: SourceContext): Exp[A] =
    PrimParse(s, m)
  def prim_convert[A, B](lhs: Exp[A])(implicit mA: Manifest[A], mB: Manifest[B], n: Numeric[A], pos: SourceContext): Exp[B] =
    PrimConvert(lhs, mA, mB, n)
  def prim_plus[A](lhs: Exp[A], rhs: Exp[A])(implicit m: Manifest[A], n: Numeric[A], pos: SourceContext): Exp[A] =
    PrimPlus(lhs, rhs, m, n)
  def prim_minus[A](lhs: Exp[A], rhs: Exp[A])(implicit m: Manifest[A], n: Numeric[A], pos: SourceContext): Exp[A] =
    PrimMinus(lhs, rhs, m, n)
  def prim_times[A](lhs: Exp[A], rhs: Exp[A])(implicit m: Manifest[A], n: Numeric[A], pos: SourceContext): Exp[A] =
    PrimTimes(lhs, rhs, m, n)
  def prim_min_value[A : Manifest] = PrimMinValue(manifest[A])
  def prim_max_value[A : Manifest] = PrimMaxValue(manifest[A])

  def prim_fp_divide[A](lhs: Exp[A], rhs: Exp[A])(implicit m: Manifest[A], f: Fractional[A], pos: SourceContext): Exp[A] =
    PrimFPDivide(lhs, rhs, m, f)
  def prim_pos_inf[A : Manifest]: Exp[A] = PrimFPPositiveInfinity(manifest[A])
  def prim_neg_inf[A : Manifest]: Exp[A] = PrimFPNegativeInfinity(manifest[A])

  def prim_integral_divide[A](lhs: Exp[A], rhs: Exp[A])(implicit m: Manifest[A], i: Integral[A], pos: SourceContext): Exp[A] =
    PrimIntegralDivide(lhs, rhs, m, i)
  def prim_mod[A](lhs: Exp[A], rhs: Exp[A])(implicit m: Manifest[A], i: Integral[A], pos: SourceContext): Exp[A] =
    PrimIntegralMod(lhs, rhs, m, i)
  def prim_bitwise_or[A : Manifest](lhs: Exp[A], rhs: Exp[A])(implicit pos: SourceContext): Exp[A] =
    PrimIntegralBitwiseOr(lhs, rhs, manifest[A])
  def prim_bitwise_and[A : Manifest](lhs: Exp[A], rhs: Exp[A])(implicit pos: SourceContext): Exp[A] =
    PrimIntegralBitwiseAnd(lhs, rhs, manifest[A])
  def prim_bitwise_xor[A : Manifest](lhs: Exp[A], rhs: Exp[A])(implicit pos: SourceContext): Exp[A] =
    PrimIntegralBitwiseXor(lhs, rhs, manifest[A])
  def prim_bitwise_not[A : Manifest](lhs: Exp[A])(implicit pos: SourceContext): Exp[A] =
    PrimIntegralBitwiseNot(lhs, manifest[A])
  def prim_left_shift[A : Manifest](lhs: Exp[A], rhs: Exp[Int])(implicit pos: SourceContext): Exp[A] =
    PrimIntegralLeftShift(lhs, rhs, manifest[A])
  def prim_right_shift_arithmetic[A : Manifest](lhs: Exp[A], rhs: Exp[Int])(implicit pos: SourceContext): Exp[A] =
    PrimIntegralRightShiftArith(lhs, rhs, manifest[A])
  def prim_right_shift_logical[A : Manifest](lhs: Exp[A], rhs: Exp[Int])(implicit pos: SourceContext): Exp[A] =
    PrimIntegralRightShiftLogical(lhs, rhs, manifest[A])

  implicit class PrimToOp[A : Manifest : Numeric](lhs: Exp[A]) {
    def to[B : Manifest]: Exp[B] = prim_convert[A, B](lhs)
  }

  /**
   * Double
   */
  def obj_double_parse_double(s: Exp[String])(implicit pos: SourceContext) = prim_parse[Double](s)
  def obj_double_positive_infinity(implicit pos: SourceContext) = prim_pos_inf[Double]
  def obj_double_negative_infinity(implicit pos: SourceContext) = prim_neg_inf[Double]
  def obj_double_min_value(implicit pos: SourceContext) = prim_min_value[Double]
  def obj_double_max_value(implicit pos: SourceContext) = prim_max_value[Double]
  def double_to_int(lhs: Exp[Double])(implicit pos: SourceContext) = lhs.to[Int]
  def double_to_float(lhs: Exp[Double])(implicit pos: SourceContext) = lhs.to[Float]
  def double_plus(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext): Exp[Double] = prim_plus(lhs,rhs)
  def double_minus(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext): Exp[Double] = prim_minus(lhs,rhs)
  def double_times(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext): Exp[Double] = prim_times(lhs,rhs)
  def double_divide(lhs: Exp[Double], rhs: Exp[Double])(implicit pos: SourceContext): Exp[Double] = prim_fp_divide(lhs,rhs)

  /**
   * Float
   */  

  def obj_float_parse_float(s: Exp[String])(implicit pos: SourceContext) = prim_parse[Float](s)
  def float_to_int(lhs: Exp[Float])(implicit pos: SourceContext) = lhs.to[Int]
  def float_to_double(lhs: Exp[Float])(implicit pos: SourceContext) = lhs.to[Double]
  def float_plus(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext): Exp[Float] = prim_plus(lhs,rhs)
  def float_minus(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext): Exp[Float] = prim_minus(lhs,rhs)
  def float_times(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext): Exp[Float] = prim_times(lhs,rhs)
  def float_divide(lhs: Exp[Float], rhs: Exp[Float])(implicit pos: SourceContext): Exp[Float] = prim_fp_divide(lhs,rhs)
   
  /**
   * Int
   */
  // case class IntDivideFrac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A]) extends Def[A]

  def obj_integer_parse_int(s: Rep[String])(implicit pos: SourceContext) = prim_parse[Int](s)
  def obj_int_max_value(implicit pos: SourceContext) = prim_max_value[Int]
  def obj_int_min_value(implicit pos: SourceContext) = prim_min_value[Int]
  def int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = prim_plus(lhs, rhs)
  def int_minus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = prim_minus(lhs, rhs)
  def int_times(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = prim_times(lhs, rhs)
  // def int_divide_frac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A])(implicit pos: SourceContext) : Exp[A] = IntDivideFrac(lhs, rhs)
  def int_divide(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = prim_integral_divide(lhs, rhs)
  def int_mod(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = prim_mod(lhs, rhs)
  def int_bitwise_or(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = prim_bitwise_or(lhs, rhs)
  def int_bitwise_and(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = prim_bitwise_and(lhs, rhs)
  def int_bitwise_xor(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = prim_bitwise_xor(lhs, rhs)
  def int_bitwise_not(lhs: Exp[Int])(implicit pos: SourceContext) = prim_bitwise_not(lhs)
  def int_to_long(lhs: Exp[Int])(implicit pos: SourceContext) = lhs.to[Long]
  def int_to_float(lhs: Exp[Int])(implicit pos: SourceContext) = lhs.to[Float]
  def int_to_double(lhs: Exp[Int])(implicit pos: SourceContext) = lhs.to[Double]
  def int_left_shift(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = prim_left_shift(lhs, rhs)
  def int_right_shift_arithmetic(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = prim_right_shift_arithmetic(lhs, rhs)
  def int_right_shift_logical(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) = prim_right_shift_logical(lhs, rhs)


  /**
   * Long
   */

  def obj_long_parse_long(s: Exp[String])(implicit pos: SourceContext) = prim_parse[Long](s)
  def long_bitwise_or(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = prim_bitwise_or(lhs,rhs)
  def long_bitwise_and(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = prim_bitwise_and(lhs,rhs)
  def long_left_shift(lhs: Exp[Long], rhs: Exp[Int])(implicit pos: SourceContext) = prim_left_shift(lhs,rhs)
  def long_right_shift_arithmetic(lhs: Exp[Long], rhs: Exp[Int])(implicit pos: SourceContext) = prim_right_shift_arithmetic(lhs,rhs)
  def long_to_int(lhs: Exp[Long])(implicit pos: SourceContext) = lhs.to[Int]
  def long_mod(lhs: Exp[Long], rhs: Exp[Long])(implicit pos: SourceContext) = prim_mod(lhs, rhs)
    
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = ({
    e match {
      case PrimParse(x, m) => prim_parse(f(x))(m, pos)
      case PrimFPPositiveInfinity(m) => prim_pos_inf(m)
      case PrimFPNegativeInfinity(m) => prim_neg_inf(m)
      case PrimMaxValue(m) => prim_max_value(m)
      case PrimMinValue(m) => prim_min_value(m)
      case PrimConvert(x, mA, mB, n) => prim_convert(f(x))(mA, mB, n, pos)
      case PrimPlus(x,y,m,n) => prim_plus(f(x),f(y))(m.asInstanceOf[Manifest[A]], n.asInstanceOf[Numeric[A]], pos)
      case PrimMinus(x,y,m,n) => prim_minus(f(x),f(y))(m.asInstanceOf[Manifest[A]], n.asInstanceOf[Numeric[A]], pos)
      case PrimTimes(x,y,m,n) => prim_times(f(x),f(y))(m.asInstanceOf[Manifest[A]], n.asInstanceOf[Numeric[A]], pos)
      case PrimFPDivide(x,y,m,n) => prim_fp_divide(f(x),f(y))(m.asInstanceOf[Manifest[A]], n.asInstanceOf[Fractional[A]], pos)
      case PrimIntegralDivide(x,y,m,n) => prim_integral_divide(f(x),f(y))(m.asInstanceOf[Manifest[A]], n.asInstanceOf[Integral[A]], pos)
      case PrimIntegralMod(x,y,m,n) => prim_mod(f(x),f(y))(m.asInstanceOf[Manifest[A]], n.asInstanceOf[Integral[A]], pos)
      case PrimIntegralBitwiseOr(x,y,m) => prim_bitwise_or(f(x),f(y))(m.asInstanceOf[Manifest[A]], pos)
      case PrimIntegralBitwiseAnd(x,y,m) => prim_bitwise_and(f(x),f(y))(m.asInstanceOf[Manifest[A]], pos)
      case PrimIntegralBitwiseXor(x,y,m) => prim_bitwise_xor(f(x),f(y))(m.asInstanceOf[Manifest[A]], pos)
      case PrimIntegralLeftShift(x,y,m) => prim_left_shift(f(x),f(y))(m.asInstanceOf[Manifest[A]], pos)
      case PrimIntegralRightShiftLogical(x,y,m) => prim_right_shift_logical(f(x),f(y))(m.asInstanceOf[Manifest[A]], pos)
      case PrimIntegralRightShiftArith(x,y,m) => prim_right_shift_arithmetic(f(x),f(y))(m.asInstanceOf[Manifest[A]], pos)

      case Reflect(PrimParse(x,m), u, es) => reflectMirrored(Reflect(PrimParse(f(x),m), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimFPPositiveInfinity(m), u, es) => reflectMirrored(Reflect(PrimFPPositiveInfinity(m), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimFPNegativeInfinity(m), u, es) => reflectMirrored(Reflect(PrimFPNegativeInfinity(m), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimMaxValue(m), u, es) => reflectMirrored(Reflect(PrimMaxValue(m), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimMinValue(m), u, es) => reflectMirrored(Reflect(PrimMinValue(m), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimConvert(x, mA, mB, n), u, es) => reflectMirrored(Reflect(PrimConvert(f(x), mA, mB, n), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimPlus(x,y,m,n), u, es) => reflectMirrored(Reflect(PrimPlus(f(x),f(y),m.asInstanceOf[Manifest[A]],n.asInstanceOf[Numeric[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimMinus(x,y,m,n), u, es) => reflectMirrored(Reflect(PrimMinus(f(x),f(y),m.asInstanceOf[Manifest[A]],n.asInstanceOf[Numeric[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimTimes(x,y,m,n), u, es) => reflectMirrored(Reflect(PrimTimes(f(x),f(y),m.asInstanceOf[Manifest[A]],n.asInstanceOf[Numeric[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimFPDivide(x,y,m,n), u, es) => reflectMirrored(Reflect(PrimFPDivide(f(x),f(y),m.asInstanceOf[Manifest[A]],n.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimIntegralDivide(x,y,m,n), u, es) => reflectMirrored(Reflect(PrimIntegralDivide(f(x),f(y),m.asInstanceOf[Manifest[A]],n.asInstanceOf[Integral[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimIntegralMod(x,y,m,n), u, es) => reflectMirrored(Reflect(PrimIntegralMod(f(x),f(y),m.asInstanceOf[Manifest[A]],n.asInstanceOf[Integral[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimIntegralBitwiseOr(x,y,m), u, es) => reflectMirrored(Reflect(PrimIntegralBitwiseOr(f(x),f(y),m.asInstanceOf[Manifest[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimIntegralBitwiseAnd(x,y,m), u, es) => reflectMirrored(Reflect(PrimIntegralBitwiseAnd(f(x),f(y),m.asInstanceOf[Manifest[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimIntegralBitwiseXor(x,y,m), u, es) => reflectMirrored(Reflect(PrimIntegralBitwiseXor(f(x),f(y),m.asInstanceOf[Manifest[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimIntegralLeftShift(x,y,m), u, es) => reflectMirrored(Reflect(PrimIntegralLeftShift(f(x),f(y),m.asInstanceOf[Manifest[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimIntegralRightShiftLogical(x,y,m), u, es) => reflectMirrored(Reflect(PrimIntegralRightShiftLogical(f(x),f(y),m.asInstanceOf[Manifest[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(PrimIntegralRightShiftArith(x,y,m), u, es) => reflectMirrored(Reflect(PrimIntegralRightShiftArith(f(x),f(y),m.asInstanceOf[Manifest[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case _ => super.mirror(e,f)
    }
  }).asInstanceOf[Exp[A]]
}

trait PrimitiveOpsExpOpt extends PrimitiveOpsExp {
  override def prim_plus[A](lhs: Exp[A], rhs: Exp[A])(implicit m: Manifest[A], n: Numeric[A], pos: SourceContext): Exp[A] = (lhs,rhs) match {
    case (Const(a),Const(b)) => unit(n.plus(a, b))
    case (Const(0),b) => b
    case (a,Const(0)) => a
    case _ => super.prim_plus(lhs,rhs)
  }

  override def prim_minus[A](lhs: Exp[A], rhs: Exp[A])(implicit m: Manifest[A], n: Numeric[A], pos: SourceContext): Exp[A] = (lhs,rhs) match {
    case (Const(a),Const(b)) => unit(n.minus(a, b))
    case (a,Const(0)) => a
    // Rewrite only valid for types with wraparound overflow
    case (Def(PrimPlus(llhs, lrhs, Manifest.Int | Manifest.Long | Manifest.Short | Manifest.Byte, _)), rhs) =>
      if (lrhs.equals(rhs))
        llhs
      else if (llhs.equals(rhs))
        lrhs
      else
        super.prim_minus(lhs,rhs)
    case _ => super.prim_minus(lhs,rhs)
  }

  override def prim_times[A](lhs: Exp[A], rhs: Exp[A])(implicit m: Manifest[A], n: Numeric[A], pos: SourceContext): Exp[A] = (lhs,rhs) match {
    case (Const(a),Const(b)) => unit(n.times(a, b))
    case (a @ Const(0),b) => a
    case (Const(1),b) => b
    case (a,b @ Const(0)) => b
    case (a,Const(1)) => a
    case _ => super.prim_times(lhs,rhs)
  }

  override def prim_fp_divide[A](lhs: Exp[A], rhs: Exp[A])(implicit m: Manifest[A], f: Fractional[A], pos: SourceContext): Exp[A] = (lhs,rhs) match {
    case (Const(a),Const(b)) if b != 0 => unit(f.div(a, b))
    // case (Const(0),b) => Const(0) // invalid because b may be 0
    case (a,Const(1)) => a
    case _ => super.prim_fp_divide(lhs, rhs)
  }

  override def prim_integral_divide[A](lhs: Exp[A], rhs: Exp[A])(implicit m: Manifest[A], i: Integral[A], pos: SourceContext): Exp[A] = (lhs,rhs) match {
    case (Const(a),Const(b)) if b != 0 => unit(i.quot(a, b))
    // case (Const(0),b) => Const(0) // invalid because b may be 0
    case (a,Const(1)) => a
    case _ => super.prim_integral_divide(lhs, rhs)
  }

  override def prim_convert[A, B](lhs: Exp[A])(implicit mA: Manifest[A], mB: Manifest[B], n: Numeric[A], pos: SourceContext) = (lhs match {
    case Const(x) => mB match {
      case Manifest.Int => unit(n.toInt(x))
      case Manifest.Long => unit(n.toLong(x))
      case Manifest.Float => unit(n.toFloat(x))
      case Manifest.Double => unit(n.toDouble(x))
    }
    case _ if mA == mB => lhs
    case Def(PrimConvert(x, mA1, mB1, _)) =>
      // Transitive conversions
      if (mA1 == Manifest.Int && (mB1 == Manifest.Float || mB1 == Manifest.Long) && mB == Manifest.Double)
        x.asInstanceOf[Exp[Int]].to[Double]
      else if (mA1 == mB) {
        // Inverse conversions
        if (mB1 == Manifest.Double && (mB == Manifest.Int || mB == Manifest.Float))
          x
        else if (mB1 == Manifest.Long && mB == Manifest.Int)
          x
        else
          super.prim_convert(lhs)(mA, mB, n, pos)
      }
    case _ => super.prim_convert(lhs)(mA, mB, n, pos)
  }).asInstanceOf[Exp[B]]
}

trait ScalaGenPrimitiveOps extends ScalaGenBase {
  val IR: PrimitiveOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PrimParse(s, m) =>
      val src = m.asInstanceOf[Manifest[_]] match {
        case Manifest.Int => src"java.lang.Integer.parseInt($s)"
        case _ => src"java.lang.$m.parse$m($s)"
      }
      emitValDef(sym, src)
    case PrimFPPositiveInfinity(m) =>
      val src = src"scala.$m.PositiveInfinity"
      emitValDef(sym, src)
    case PrimFPNegativeInfinity(m) =>
      val src = src"scala.$m.NegativeInfinity"
      emitValDef(sym, src)
    case PrimMaxValue(m) =>
      val src = src"scala.$m.MaxValue"
      emitValDef(sym, src)
    case PrimMinValue(m) =>
      val src = src"scala.$m.MinValue"
      emitValDef(sym, src)
    case PrimPlus(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case PrimMinus(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case PrimTimes(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case PrimFPDivide(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case PrimConvert(lhs, _, mB, _) => emitValDef(sym, quote(lhs) + ".to" + mB)
    // case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case PrimIntegralDivide(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case PrimIntegralMod(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case PrimIntegralBitwiseOr(lhs, rhs,_) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case PrimIntegralBitwiseAnd(lhs, rhs,_) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case PrimIntegralBitwiseXor(lhs, rhs,_) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case PrimIntegralLeftShift(lhs, rhs,_) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case PrimIntegralRightShiftArith(lhs, rhs, _) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
    case PrimIntegralRightShiftLogical(lhs, rhs, _) => emitValDef(sym, quote(lhs) + " >>> " + quote(rhs))
    case PrimIntegralBitwiseNot(lhs, _) => emitValDef(sym, "~" + quote(lhs))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenPrimitiveOps extends CLikeGenBase {
  val IR: PrimitiveOpsExp
  import IR._

  def posInfConstant(m: Manifest[_]): String =
    throw new GenerationFailedException(s"Constant for positive infinity of type $m unknown")
  def negInfConstant(m: Manifest[_]): String =
    throw new GenerationFailedException(s"Constant for negative infinity of type $m unknown")

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case PrimParse(s, m) =>
        val src = m.asInstanceOf[Manifest[_]] match {
          case Manifest.Int => src"atoi($s.c_str())"
          case Manifest.Long => src"strtod($s.c_str(),NULL)"
          case Manifest.Float => src"strtof($s.c_str(),NULL)"
          case Manifest.Double => src"strtod($s,NULL)"
        }
        emitValDef(sym, src)
      case PrimFPPositiveInfinity(m) =>
        emitValDef(sym, posInfConstant(m))
      case PrimFPNegativeInfinity(m) =>
        emitValDef(sym, negInfConstant(m))
      case PrimMaxValue(m) =>
        val src = m.asInstanceOf[Manifest[_]] match {
          case Manifest.Int => "INT32_MAX"
          case Manifest.Long => "INT64_MAX"
          case Manifest.Float => "FLT_MAX"
          case Manifest.Double => "DBL_MAX"
        }
        emitValDef(sym, src)
      case PrimMinValue(m) =>
        val src = m.asInstanceOf[Manifest[_]] match {
          case Manifest.Int => "INT32_MIN"
          case Manifest.Long => "INT64_MIN"
          case Manifest.Float => "FLT_MIN"
          case Manifest.Double => "DBL_MIN"
        }
        emitValDef(sym, src)
      case PrimConvert(lhs, _, mB, _) =>
        val tpe = mB.asInstanceOf[Manifest[_]] match {
          case Manifest.Int => "int32_t"
          case Manifest.Long => "int64_t"
          case Manifest.Float => "float"
          case Manifest.Double => "double"
        }
        emitValDef(sym, src"($tpe)$lhs")
      case PrimPlus(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
      case PrimMinus(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
      case PrimTimes(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
      case PrimFPDivide(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      // case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      case PrimIntegralDivide(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      case PrimIntegralMod(lhs, rhs, _, _) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
      case PrimIntegralBitwiseOr(lhs, rhs, _) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
      case PrimIntegralBitwiseAnd(lhs, rhs, _) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
      case PrimIntegralBitwiseXor(lhs, rhs, _) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
      case PrimIntegralLeftShift(lhs, rhs, _) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
      case PrimIntegralRightShiftArith(lhs, rhs, _) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
      case PrimIntegralRightShiftLogical(lhs, rhs, m) =>
        val tpe = m.asInstanceOf[Manifest[_]] match {
          case Manifest.Int => "uint32_t"
          case Manifest.Long => "uint64_t"
        }
        emitValDef(sym, src"($tpe)$lhs >> $rhs")
      case PrimIntegralBitwiseNot(lhs, _) => emitValDef(sym, "~" + quote(lhs))
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenPrimitiveOps extends CudaGenBase with CLikeGenPrimitiveOps {
  val IR: PrimitiveOpsExp
  import IR._

  override def posInfConstant(m: Manifest[_]): String = m match {
    case Manifest.Float => "__int_as_float(0x7f800000)"
    case Manifest.Double => "__longlong_as_double(0x7ff0000000000000ULL)"
    case _ => super.posInfConstant(m)
  }

  override def negInfConstant(m: Manifest[_]): String = m match {
    case Manifest.Float => "__int_as_float(0xff800000)"
    case Manifest.Double => "__longlong_as_double(0xfff0000000000000ULL)"
    case _ => super.negInfConstant(m)
  }
}

trait OpenCLGenPrimitiveOps extends OpenCLGenBase with CLikeGenPrimitiveOps

trait CGenPrimitiveOps extends CGenBase with CLikeGenPrimitiveOps {
  val IR: PrimitiveOpsExp
  import IR._

  override def posInfConstant(m: Manifest[_]): String = "INFINITY"

  override def negInfConstant(m: Manifest[_]): String = "-INFINITY"
}
