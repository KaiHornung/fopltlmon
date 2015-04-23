/*******************************************************************************
 * This is part of fopltlmon (https://github.com/kaihornung/fopltlmon).
 *  
 *  Copyright (c) 2015 by Kai Hornung <development@khornung.com>
 *   
 *  Fopltlmon is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *   
 *  Fopltlmon is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *   
 *  You should have received a copy of the GNU General Public License
 *  along with fopltlmon.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package monitor

import control._
import datatype._
import datatype.Types._

import scala.collection._

object Util {
  
  /**
   * Create the standard structure
   * 
   * Add custom interpreted preditates and/or functions here
   */
  def getStandardStructure():Structure = {
    val struct = new Structure()
    
    struct.addFunction  ("+" , (args => args match { case Vector(Some(a: Int),Some(b: Int)) => a+b  case _ => None }))
    struct.addFunction  ("-" , (args => args match { case Vector(Some(a: Int),Some(b: Int)) => a-b  case _ => None }))
    struct.addFunction  ("*" , (args => args match { case Vector(Some(a: Int),Some(b: Int)) => a*b  case _ => None }))

    struct.addIPredicate("<" , (args => args match { case Vector(Some(a: Int),Some(b: Int)) => a<b  case _ => false}))
    struct.addIPredicate(">" , (args => args match { case Vector(Some(a: Int),Some(b: Int)) => a>b  case _ => false}))
    struct.addIPredicate("<=", (args => args match { case Vector(Some(a: Int),Some(b: Int)) => a<=b case _ => false}))
    struct.addIPredicate(">=", (args => args match { case Vector(Some(a: Int),Some(b: Int)) => a>=b case _ => false}))
    struct.addIPredicate("=" , (args => args match { case Vector(Some(a)     ,Some(b))      => a==b case _ => {println(args);  false}}))
    struct.addIPredicate("!=", (args => args match { case Vector(Some(a)     ,Some(b))      => a!=b case _ => false}))
    
    /*
     * Example for a function: Needs to be of type Vector[Option[Any]] => Any 
     * struct.addFunction("plus", (args => args.foldLeft(0){ case (n,Some(v:Int)) => n + v case (n,_) => n }))
     */
    
    List("p","q","r","s","t","u","v","w").foreach( struct.addUPredicate )  
    
    List("op","R1","R2","loc","store","load","storeR","loadR","reg").foreach( struct.addUPredicate )
    
    List("proc","noncrit","waiting","crit","exit","startWaiting","entering").foreach( struct.addUPredicate )
    
    return struct
  }
  
  /**
   * Spawn new automaton for given formula under the given assignment and process the initial event. If already spawned in this step, return reference
   */
  def spawn(phi: Formula, assignment: Assignment, initialEvent: Event):SMA = { 
    Pool.spAutomata.get((phi.toString,assignment)) match {
      case None      => { 
        val newSMA = new SMA(Pool.prototypes(phi.toString),assignment)
        Pool.spAutomata.put((phi.toString,assignment), newSMA)
        newSMA.process(initialEvent)
        return newSMA }
      case Some(sma) => return sma  //The same automaton is already spawned in this step, don't spawn again, only return reference
    }
  }
  
  /**
   * Helper to extend assignment, i.e. beta[(d1,...,dn) / (x1,...,xn)]
   */
  def extendAssignment(assignment: Assignment, updates: (Vector[Term],Vector[Any]) ): Assignment = {    
    val newAssignment = mutable.Map[String,Any]() ++ assignment 
    updates match { case (variables,values) => variables.zipWithIndex.foreach { case (Var(x),i) => newAssignment.put(x,values(i)) case _ => } }
    return newAssignment 
  }
  
  /**
   * Compute local result for a final valuation
   */
  def localResult(valuation: Valuation): Result = valuation.map { case (par,(_,m,_)) => (par -> m) }
  
  /**
   * Compute valuation conjunction for two results under phi
   */
  def merge(res1: Result, res2: Result, phi: Formula): Result = { phi.parameters.map { case (par,upward) => (res1.get(par),res2.get(par),upward) match {
    case (Some(v1),Some(v2),true)  => (par -> Math.max(v1, v2))
    case (Some(v1),Some(v2),false) => (par -> Math.min(v1, v2))
    case (Some(v1),None    ,_    ) => (par -> v1)
    case (None    ,Some(v2),_    ) => (par -> v2)
    case (None    ,None    ,up   ) => if (up) (par -> 0) else (par -> Int.MaxValue)
  } } }
  
  /**
   * Get best result wrt. lexicographical parameter ordering
   */
  def bestResult(results: List[Result],phi: Formula): Result = {
    assert(results.size > 0)
    val sortedKeys = results.head.keySet.toList.sorted
    var best = results
    sortedKeys.foreach { key => best = best.filter { res1 => best.forall { res2 => if (phi.parameters(key)) res2(key) >= res1(key) else res2(key) <= res1(key)} } }
    
    assert(best.size > 0)
    return best.head
  }
}