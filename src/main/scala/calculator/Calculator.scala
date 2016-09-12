package calculator

import scala.collection.mutable.LinkedHashMap

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  
  val evaluatedExpr: LinkedHashMap[String, List[String]] = LinkedHashMap()
       
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
       evaluatedExpr.clear()
//       println("computeValues " + evaluatedExpr.size);
       for {
         (k,v) <- namedExpressions
         } yield { 
           val vVal = v()
           val refNameList = evaluatedExpr.getOrElseUpdate(k, List())
           (k -> Signal{ eval(k, vVal, namedExpressions) })
         }
     }

  def hasCyclicDependency(lName : String, rName : String) : Boolean = {
    println("check hasCyclicDependency " + lName + " " + rName);
    def iter(l: String, r: String) : Boolean = {
      if (l == r) {
          println("found cyclic dependency " + l + " == " r)
        true
      } else {
        val v = evaluatedExpr.find(p => { 
//          println("looking for: " + l + " in key: " + p._1 ); 
          p._2.contains(l) 
        })
        if (v.isEmpty) {
//          println("cyclic dependency not found");
          false
        } else {
//          println("found " + l + " into " + v.get._1 + " key ")
          iter(v.get._1, rName)
        }
      }
    }
    iter(lName, rName)
  }
  
  def eval(k: String, expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    expr match {
           case l:Literal => { 
//             println("Literal k: " + k + " value: " + l.v)
             l.v 
           }
           case plus:Plus => eval(k, plus.a, references) + eval(k, plus.b, references)
           case minus:Minus => eval(k, minus.a, references) - eval(k, minus.b, references)
           case times:Times => eval(k, times.a, references) * eval(k, times.b, references)
           case divide:Divide => eval(k, divide.a, references) * eval(k, divide.b, references)
           case r:Ref => { 
//            val ref = getReferenceExpr(r.name, references)
//            eval(k, ref, references - r.name)

             println("Ref k: " + k + " r.name: " +  r.name)
             if (hasCyclicDependency(k, r.name)) {
               Double.NaN
             } else {
               val expr = getReferenceExpr(r.name, references)
               val refNameList = evaluatedExpr.get(k).get
               if (!refNameList.contains(r.name))
                 evaluatedExpr.put(k, r.name :: refNameList)
               eval(k, getReferenceExpr(r.name, references), references)
             }
           }
           case _ => { println("case not handled"); 0.0 }
     }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
