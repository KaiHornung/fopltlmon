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
import monitor.Util._
import control._
import control.Config._
import scala.collection._
import java.util.HashMap

/**
 * "State" of the measuring automaton. States are needed to detect early acceptance. They behave independently of the considered runs that are needed for the measuring component
 */
class State(val automaton: SMAPrototype, at: Set[Atom]) {
  
  val isTrapState            = at.size == 0
  
  //toString and logging
  val id                     = if (isTrapState) Int.MaxValue else StateNameGenerator.getNext(automaton.id)
  override lazy val toString = "q" + automaton.id + "." + (if (isTrapState) "T" else id)
  def printDetails           = Log.myprintln("  * " + this.detailedToString + ": " + atoms.mkString("{ ", " , ", " }")  + transitions.mkString("\n      - ", "\n      - ", ""))
  lazy val detailedToString  = this.toString.padTo(5," ").mkString("") + (if (this.isFinal) " \u2208 \u03A9" else { if (this.isAccepting) " \u2208 F" else "    " })
  
  val atoms                  = mutable.SortedSet.empty[Atom](AtomOrdering) ++ at
  
  //local acceptance condition
  lazy val isAccepting    = atoms.exists { _.isAccepting } 
  var isFinal             = false
  lazy val isGloballyAccepting = (atoms.exists { atom => atom.isAccepting } 
                            && atoms.forall { atom => atom.isParameterFree } 
                            && automaton.allPossibleEvents.forall{event => automaton.phi.abstraction.closure.filter(_.isInstanceOf[Quantification]).forall {quant => delta(event).atoms.exists(_.formulae.contains(quant))} })
  
                            
  //outgoing transitions (lazily evaluated and cached)
  private val transitions = mutable.HashMap[String,State]()
  
  /**
   * Compute and cache successor state for event
   */
  def delta(event: Formulae): State = {
    val eventString = if (event.isEmpty) "default" else event.mkString(";")
    
    transitions.get(eventString) match {
      case Some(trans) => return trans
      case None        => {
                
        val atomMap = Map[String,Set[Atom]]() ++ atoms.map { atom => (atom.toString -> atom.delta(event)) }
        val atomSet = atomMap.foldLeft(mutable.SortedSet.empty[Atom](AtomOrdering)) { case (set,(atomString,successors)) => set ++= successors }
        
        transitions.put(eventString, automaton.stateForAtomSet(atomSet))
                
        return delta(event)
      }
    }
  }
  
  /**
   * Override methods for atom comparison:
   * Two atoms are considered equal whenever they have the same string representation
   */
  override def equals(o: Any) = o match {
    case that: State => toString == that.toString
    case           _ => false
  }
  override def hashCode = toString.hashCode
}

/**
 * Helper to generate sma id and ordering for sorted collections
 */
object StateNameGenerator { val indices = mutable.HashMap[Int,Int]() ; def getNext(prototypeIndex: Int): Int = { indices.get(prototypeIndex) match {case None => indices.put(prototypeIndex,1) ; return 0 case Some(x) => indices.put(prototypeIndex,x+1); return x } } }
object StateOrdering extends Ordering[State] { def compare(a:State, b:State) = if (a.automaton.id == b.automaton.id) a.id compare b.id else a.automaton.id compare b.automaton.id}
