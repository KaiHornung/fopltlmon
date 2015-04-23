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
import scala.collection._
import scala.language.implicitConversions
import scala.language.reflectiveCalls
import control._
import control.Config._
import scala.util.control.Breaks._

/**
 * Automaton prototype for the input formula under the structure. Neccessary, so sub-automata for the same formula don't need to recompute the automaton structure.
 */
class SMAPrototype(val phi: Formula, val structure: Structure) {
  //States of this automaton prototype, compute all at once
  val id                  = SMAPrototypeNameGenerator.getNext
  val stateMap            = mutable.Map[String,State]()
  
  val allPossibleEvents   = phi.closure.filter(_.isInstanceOf[Predicate]).foldLeft(Set[Formulae]( mutable.SortedSet.empty[Formula](FormulaOrdering) )) { case (events,formula) => (events.map { event => event + formula }) ++ (events.map { event => event + Negate(formula) }) } 
  val initialAtom         = new Atom(this,Set[Formula](Next(phi)))
  val atoms               = initializeAtoms
  val initialState        = stateForAtomSet(SortedSet.empty[Atom](AtomOrdering) + initialAtom)
  
  def states = mutable.SortedSet.empty[State](StateOrdering) ++= stateMap.values
  
  classifyStates
  computeTransitions  //Slow? Neccessary?
  
  if (debug) { Log.myprintln("")  ; printAtoms }
  if (debug) { printStates ; Log.myprintln("") }
  
  //Helpers, Boolean implication and equivalence
  implicit def extendedBoolean(a : Boolean) = new { def implies(b : => Boolean) = { !a || b } ; def iff(b : => Boolean) = { !a ^ b } }
  
  /**
   * Computes all transitions
   */
  def computeTransitions = atoms.foreach { atom => allPossibleEvents.foreach { event => atom.delta(event) } }
  
  /**
   * Optimization: Classify final states, i.e. states that will remain accepting!
   */
  def classifyStates = {
    val transitions = mutable.Map[State,Set[State]]()
    val allStates   = states
    allStates.foreach { state => transitions.put(state, allPossibleEvents.map { event => state.delta(event) }) }
    
    val table = mutable.Map[(State,State),Boolean]()
    allStates.foreach { state1 => allStates.foreach { state2 => table.put((state1,state2), state1.isGloballyAccepting != state2.isGloballyAccepting) } }
    
    var newMarks = 0
    do {
      newMarks = 0
      table.foreach { 
        case ((state1,state2),false) => if (! transitions(state1).forall { succ1 => transitions(state2).forall { succ2 => !table((succ1,succ2)) } }) { table.put((state1,state2),true) ; newMarks = newMarks + 1 }
        case  _ =>
      }
    } while( newMarks > 0 )
            
    assert(table.keys.forall { case (s1,s2) => table(s1,s2) iff table(s2,s1) })  //Table is symmetric
        
    allStates.foreach { state => if (state.isGloballyAccepting && table.exists{ case ((s1,s2),false) => s1 == state ^ s2 == state case _ => false }) state.isFinal = true }
  }
  
  /**
   * Compute all atoms
   */
  def initializeAtoms: SortedSet[Atom] = {
    val allMandatory = if (phi.closure.contains(True)) mutable.Set[Formula](True) else mutable.Set[Formula]();  //Formulae that need to be present in all reachable states
    phi match {
      case Release (False,psi) => allMandatory.add(phi) ; allMandatory.add(phi.abstraction)                     //Cannot be released, therefore present in every atom
      case _                   => 
    }
    
    var iterator = phi.closure.subsets.filter { subset => (
        allMandatory.subsetOf(subset)                                                                                                           // At.1.4 & optimization
        &&
        (phi.closure forall {          //This is an optimization, for atomic formulae implicit, for quantification needed                       // At.1.3 & At.8
          formula => if (formula.isParametric) (subset.contains(formula) implies subset.contains(formula.abstraction)) else (subset.contains(formula) iff !subset.contains(Negate(formula)))
        }))
    }
        
    //Local consistency conditions and unambiguity conditions,
    iterator = (iterator.filter { subset =>
      phi.closure forall { form => form match {
        case Conjunction(phi,psi) => subset.contains(form)   iff    ( subset.contains(phi) && subset.contains(psi) )                                                                                     //At.1.1
        case Disjunction(phi,psi) => subset.contains(form)  implies ((subset.contains(phi) && subset.contains(phi.abstraction)) || (subset.contains(Negate(phi.abstraction)) && subset.contains(psi)))   //At.1.2 & At.5
        case Until      (phi,psi) => (subset.contains(form) implies (subset.contains(psi ) || (subset.contains(phi) && !subset.contains(psi.abstraction)))                                              //At 2 & At.6
                                  && (subset.contains(psi ) implies  subset.contains(form) ))
        case Release    (phi,psi) => (subset.contains(form) implies (subset.contains(psi ) && (subset.contains(phi) || subset.contains(Negate(phi.abstraction))))  
                                  && ((subset.contains(phi) && subset.contains(psi)) implies  subset.contains(form) ))                                                                                  //At.3 & At.7
        case PGlobally  ( _ ,phi) => subset.contains(form) implies subset.contains(phi)                                                                                                                                               //At.4
        case _                    => true
      }}
    })
                
    val atomSet = mutable.SortedSet.empty[Atom](AtomOrdering)
    iterator.foreach { formulaSet => atomSet.add(new Atom(this,formulaSet)) }
    return atomSet    
  }
  
  /**
   * Create States for atom sets
   */
  def stateForAtomSet(atomSet: SortedSet[Atom]): State = {
    stateMap.get(atomSet.mkString(":")) match {
      case Some(state) => return state
      case None        => { val newState = new State(this,atomSet)
                            stateMap.put(atomSet.mkString(":") , newState) 
                            this.allPossibleEvents.foreach { event => newState.delta(event) }
                            return stateForAtomSet(atomSet) }
    }
  }
  
  //Helper function for printing all states
  def printAtoms  = { Log.myprintln("Atoms  in Automaton " + id + " for formula: " + phi) ; atoms. foreach  { _.printDetails } }
  def printStates = { Log.myprintln("States in Automaton " + id + " for formula: " + phi) ; states.foreach { _.printDetails } }
}

object SMAPrototypeNameGenerator {
  var index = 0
  def getNext:Int = { index = index + 1 ; return index }
}