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

/**
 * Class Run
 * Represents the current state of a possibly accepting run. It is only present for one point in time and is then replaced by its successor(s)
 */
class Run (val sma: SMA, val atom: Atom, val valuation: Valuation, val obligations: Obligations, val finalObligationResult: Result){

  val id = RunNameGenerator.getNext(sma.id)
   
  /**
   * Event processing
   * @par event: The event that needs to be processed
   * @return   : The set of yielded runs
   */
  def processEvent(eventFormulae: Formulae, event: Event): List[Run] = {    
    val newRuns = mutable.Set[Run]()
    
    atom.delta(eventFormulae).foreach { successor =>
      val newObligations = successor.lambda(event,sma.assignment)
      if( newObligations.isDefined ) {                   
        val obligationPartition = (obligations ++ newObligations.get).partition(_.isFinal)
        
        val newFinalObligationResult = { 
          if (obligationPartition._1.size > 0) { 
            if (finalObligationResult == null)
              obligationPartition._1.tail.foldLeft(obligationPartition._1.head.result) { case (res,obl) => Util.merge(res, obl.result, sma.prototype.phi) } 
            else
              obligationPartition._1.foldLeft(finalObligationResult) { case (res,obl) => Util.merge(res, obl.result, sma.prototype.phi) } 
          } 
          else finalObligationResult 
          
        }
        
        val newRun = new Run(sma,successor,successor.gamma(valuation),obligationPartition._2,newFinalObligationResult)
        
        newRuns.add(newRun)                                  //Eliminates duplicate runs by the equals definition below
      }
    }
    
    if (Config.debug) Log.myprintln("Run " + this + (if (newRuns.size > 0) " yields new runs " + newRuns.mkString(" , ") else " is trapped." ) )
    
    return newRuns.toList.sorted(RunOrdering)                //Returns the new runs sorted by id
  }
  
  /**
   * Properties of this run
   */
  def isAccepting     : Boolean = atom.isAccepting && obligations.forall { case ForallObligation(list) => list.forall(_.isAccepting) case ExistsObligation(list) => list.exists(_.isAccepting) }
  lazy val isFinal    : Boolean = atom.isAccepting && obligations.forall { case ForallObligation(list) => list.forall(_.isFinal    ) case ExistsObligation(list) => list.exists(_.isFinal    ) }
  lazy val isTrapped  : Boolean = obligations.exists { case ForallObligation(list) => list.exists(_.isTrapped) case ExistsObligation(list) => list.forall(_.isTrapped) }
  
  lazy val result     : Result  = obligations.foldLeft(if (finalObligationResult != null) Util.merge(localResult,finalObligationResult,sma.prototype.phi) else localResult){ case (res,obl) => Util.merge(res, obl.result, sma.prototype.phi) }
  lazy val localResult: Result  = Util.localResult(valuation)
  
  /**
   * Debugging
   */
  override lazy val toString = sma.prototype.id + "." + sma.id + "." + id
  lazy val details           = (if (isAccepting) "Accepting " else "") + "Run " + this + " in Atom " + atom + ( if(valuation.isEmpty && obligations.isEmpty) "" else " with:\n" + ( if (!valuation.isEmpty) "    valuation  : " + valuation.mkString("[ " , " , " , " ]") + "\n" else "") + ( if (!obligations.isEmpty) "    obligations: " + obligations.map { obl => obl.subAutomata.map{s => s.prototype.id + "." + s.id}.mkString(obl.symbol + "(", ",", ")") }.mkString(" /\\ ")  + "\n" else "" ))// + ( if (!finalObligations.isEmpty) "   fobligations: " + finalObligations.map { obl => obl.subAutomata.map{s => s.prototype.id + "." + s.id}.mkString(obl.symbol + "(", ",", ")") }.mkString(" /\\ ")  + "\n" else "" ) ) 
 
  /**
   * Override methods for run comparison:
   * Two runs are considered equal whenever they 
   * - are in the same atom of the same automaton
   * - have the same valuation and
   * - have the same obligations
   */
  override def equals(o: Any) = o match {
    case that: Run => sma == that.sma && atom == that.atom && valuation == that.valuation && obligations == that.obligations
    case         _ => false
  }
  override def hashCode = (sma + "|" + atom + "|" + valuation + "|" + obligations).hashCode
}

/**
 * Helper to generate run id and ordering for sorted collections
 */
object RunNameGenerator { val indices = mutable.HashMap[Int,Int]() ; def getNext(smaId: Int): Int = { indices.get(smaId) match {case None => indices.put(smaId,1) ; return 1 case Some(x) => indices.put(smaId,x+1); return x+1 } } }
object RunOrdering extends Ordering[Run] { def compare(a:Run, b:Run) = if (a.sma == b.sma) (a.id compare b.id) else (a.sma.id compare b.sma.id) }