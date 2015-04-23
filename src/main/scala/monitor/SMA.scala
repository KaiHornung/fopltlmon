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

import datatype._
import datatype.Types._
import control._
import scala.collection._
import Util._

/**
 * Automaton instance of the given prototype under the assignment.
 */
class SMA(val prototype: SMAPrototype, val assignment: Assignment) {
  
  val id                     = SMANameGenerator.getNext(prototype.id)
  val start                  = Log.step
  
  var runs                   = List[Run](new Run(this,prototype.initialAtom,initialValuation(prototype.phi),initialObligations,null))
  var currentState           = prototype.initialState
  
  /**
   * Event processing
   */
  def process(event: Event) = {
    val eventFormulae = mutable.SortedSet.empty[Formula](FormulaOrdering) ++ prototype.phi.atomicSubformulae.filter(_.isInstanceOf[Predicate]).map { sf => if (sf.asInstanceOf[Predicate].interpret(prototype.structure, event, assignment)) sf else Negate(sf) } 
    
    currentState = currentState.delta(eventFormulae)
    
    if(! currentState.isTrapState){
      runs = runs.foldLeft(List[Run]()){ case (newRuns,run) => newRuns ++ run.processEvent(eventFormulae,event).filterNot(_.isTrapped) }
    
      runs.filter(_.isAccepting).foreach { acceptingRun => 
        runs = runs.filter { run => run.atom != acceptingRun.atom || run == acceptingRun }
      }
    } else {
      runs = List[Run]()
    }
    
    assert(runs.map(_.atom).forall{currentState.atoms.contains})
  }
  
  /**
   * Cached Properties
   */
  def isAccepting : Boolean  = {
    if (false && Log.step == isAcceptingLast) 
      return isAcceptingCache 
    else { 
      isAcceptingLast = Log.step
      if (runs.exists(_.isAccepting)) 
        isAcceptingCache = true
      else 
        isAcceptingCache = false
      return isAcceptingCache
    }}
  private var isAcceptingCache = false ; private var isAcceptingLast  = -1
  
  def isTrapped   : Boolean  = { 
    if (isTrappedCache || Log.step == isTrappedLast) 
      return isTrappedCache 
    else { 
      isTrappedLast = Log.step
      if (runs.forall(_.isTrapped)) {
         isTrappedCache = true
         return true
      }
      else
        return false
    }}
  private var isTrappedCache = false ; private var isTrappedLast  = -1
  
  def isFinal: Boolean = {
    if (isFinalCache || Log.step == isFinalLast) 
      return isFinalCache 
    else { 
      isFinalLast = Log.step                    //Accepting                  All counters inactive                All obligations final
      if (currentState.isFinal && runs.exists{ run => run.isAccepting && !run.valuation.exists(_._2._3) && run.obligations.forall(_.isFinal)}) {
         
         if (Config.printResults) printDetailedResult
        
         isFinalCache = true
         return true
      }
      else
        return false
    }}
  private var isFinalCache = false ; private var isFinalLast  = -1
  
    
  /**
   * Result compution and processing
   */
  def result: Option[Result] = { val acceptingRuns = runs.filter(_.isAccepting) ; if (acceptingRuns.isEmpty) return None else {Some(acceptingRuns.head.result) } }
  
  def printDetailedResult: Unit = {
    if (isAccepting) {
      val acceptingRuns = runs.filter(_.isAccepting)
            
      val acceptingRun  = acceptingRuns.head    //Get the unique accepting atom
      
      if (prototype.phi.closure.exists(_.isInstanceOf[ParametricOperator])) {
        val localResult  = acceptingRun.localResult
      
        Config.resultPrintWriter.print(start.toString.padTo(15, " ").mkString(""))
        Config.resultPrintWriter.print(" | ")
        Config.printVariables. foreach { v => Config.resultPrintWriter.print((if (assignment.get(v) == None) "-" else assignment(v)).toString.padTo(12, " ").mkString("")) }
        Config.resultPrintWriter.print(" | ")
        Config.printParameters.foreach { p => Config.resultPrintWriter.print((if (acceptingRun.valuation.get(p).isEmpty) "-" else localResult(p)).toString.replaceFirst(Int.MaxValue.toString, "\u221E").padTo(10, " ").mkString("")) }
      
        Config.resultPrintWriter.println("")
      }
      
    } else println("No result, check verbose file!")
  }
  
  /**
   * Debugging
   */
  override def toString = prototype.id + "." + id + " - " + assignment.mkString("[" , " , " , "]")
  def details           = (if(isAccepting) "Accepting " else "") + "SMA " + this + " in state " + currentState + " started at " + start + " has runs:" + runs.map(_.details.replaceAll("\n", "\n    ")).mkString("\n  * ", "\n  * ", "\n")
  
  /**
   * Override methods for sma comparison:
   * Two smas are considered equal whenever they have the same string representation
   */
  override def equals(o: Any) = o match {
    case that: SMA => toString == that.toString
    case         _ => false
  }
  override def hashCode = toString.hashCode
}

/**
 * Helper to generate sma id and ordering for sorted collections
 */
object SMANameGenerator { val indices = mutable.HashMap[Int,Int]() ; def getNext(prototypeIndex: Int): Int = { indices.get(prototypeIndex) match {case None => indices.put(prototypeIndex,1) ; return 1 case Some(x) => indices.put(prototypeIndex,x+1); return x+1 } } }
object SMAOrdering extends Ordering[SMA] { def compare(a:SMA, b:SMA) = if (a.prototype.id == b.prototype.id) a.id compare b.id else a.prototype.id compare b.prototype.id}