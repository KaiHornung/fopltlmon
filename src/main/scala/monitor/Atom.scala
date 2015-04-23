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
 * Actual state of the measuring automaton.
 */
class Atom(val automaton: SMAPrototype, forms: Set[Formula]) {
  
  val id       = AtomNameGemerator.getNext(automaton.id)
  val formulae = mutable.SortedSet.empty[Formula](FormulaOrdering) ++ forms
  
  
  /**
   * Cached properties
   */
  lazy val isParameterFree = formulae.forall { form => !form.isParametric }
  
  lazy val isAccepting     = this != automaton.initialAtom && formulae.forall { 
                                        case PEventually(_,phi) => Config.weak || formulae.contains(phi) 
                                        case Until(_,psi)       => Config.weak || formulae.contains(psi) 
                                        //case Next(_)            => false 
                                        case _                  => true }
  
  
  
  /**
   * delta: returns all successors of this atom                                 
   */
  def delta(event: Formulae): SortedSet[Atom] = { deltaMap.get(event.mkString(";")) match {
      case None      => deltaMap.put(event.mkString(";"), computeDelta(event)) ; return delta(event)
      case Some(set) => return set 
    }
  }
  //Cached Successors
  private val deltaMap = mutable.HashMap[String,SortedSet[Atom]]()
  //Helper to compute successors
  private def computeDelta(event: Formulae): SortedSet[Atom] = {
    val mandatory = mutable.SortedSet.empty[Formula](FormulaOrdering) ++ event
    
    this.mandatoryDelta.foreach { m => mandatory.add(m) }
    
    val ret = mutable.SortedSet.empty[Atom](AtomOrdering)
    automaton.atoms foreach { atom =>
          if ((mandatory forall atom.formulae.contains) && (ret.forall { subatom => !subatom.formulae.subsetOf(atom.formulae) }))
            ret.add(atom)
        }
    return ret
  }
  //Cached mandatory formulae for successors
  lazy val mandatoryDelta: Formulae = { val ret = mutable.SortedSet.empty[Formula](FormulaOrdering)
                                      for (form <- formulae) {
                                        form match {
                                          case Next(phi)             => ret.add(phi)
                                          case Until(phi,psi)        => if (!formulae.contains(psi)) ret.add(form)
                                          case Release(phi,psi)      => if (!formulae.contains(phi)) ret.add(form)
                                          case PEventually(par, phi) => if (!formulae.contains(phi)) ret.add(form)
                                          case _                     =>
                                        }
                                      }
                                      ret
                                     }
  
  /**
   * gamma: returns updated valuation for incoming transitions
   */
  def gamma(valuation: Valuation): Valuation = valuation.map { case (name, (n,m,a)) => gammaUpdates.get(name) match {
      case None     => (name -> (n,m,false))
      case Some((situation,upward)) => upward match {
        case true  => situation match {
          case UpdateSituation.BothPresent   => name -> (  0  , Math.max(n,m) , true )
          case UpdateSituation.ParentPresent => name -> ( n+1 ,      m        , true )
          case _                             => name -> (  n  ,      m        , false)
        }
        case false  => situation match {
          case UpdateSituation.BothPresent | UpdateSituation.ParentPresent  => name -> ( if (a)  0  else n+1 , m , true )
          case UpdateSituation.ChildPresent                                 => name -> ( if (a) n+1 else  0  , m ,  a   )
          case UpdateSituation.NonePresent                                  => name -> ( 0 , if (a) Math.min(m,n) else m , false )
        }
      }
    } 
  }
  //Cached update actions for gamma
  private lazy val gammaUpdates = {
    val updates = mutable.Map[String,UpdateAction]()
    
    automaton.phi.closure.foreach { formula => formula match {
      case PEventually(par,phi) => updates.put(par, (if (formulae.contains(formula)) (if (formulae.contains(phi)) UpdateSituation.BothPresent else UpdateSituation.ParentPresent) else (if (formulae.contains(phi)) UpdateSituation.ChildPresent else UpdateSituation.NonePresent),true ))
      case PGlobally  (par,phi) => updates.put(par, (if (formulae.contains(formula)) (if (formulae.contains(phi)) UpdateSituation.BothPresent else UpdateSituation.ParentPresent) else (if (formulae.contains(phi)) UpdateSituation.ChildPresent else UpdateSituation.NonePresent),false)) 
      case _                    => 
    } }
    
    updates
  }
  
  /**
   * lambda: returns spawning obligations for incoming transitions
   */
  def lambda(event: Event, assignment: Assignment): Option[Obligations] = {
    val obligations = mutable.MutableList[Obligation]()
    
    lambdaQuantificatios.foreach { case q:Quantification => {
      val eventGuarded      = (event.filter { case (pred,_) => pred == q.guard.name })
      val assignmentUpdates = eventGuarded.map {case (_,args) => (q.guard.arguments, args) }.toList
      
      q.asInstanceOf[Quantification] match {
        case Forall(guard,phi) => if (!eventGuarded.isEmpty) obligations += Obligation(ForallInstantiation(phi,assignmentUpdates),assignment,event)
        case Exists(guard,phi) => if  (eventGuarded.isEmpty) return None else obligations += Obligation(ExistsInstantiation(phi,assignmentUpdates),assignment,event)
      }
    } case _ => }
        
    return Some(obligations.toList)
  }
  //Cached quantifications for lambda
  private lazy val lambdaQuantificatios: Formulae = {
    val skip            = mutable.Set[Formula]()
    
    val quantifications = formulae.filter { form => form match {
      case q:Quantification => if (form.isParametric) skip.add(form.abstraction) ; true
      case _                => false
    } }

    (quantifications -- skip)
  }  
  
   
  
  /**
   * Debugging
   */
  override lazy val toString = "a" + automaton.id + "." + id
  lazy val details           = "  * " + this.toString.padTo(4, " ").mkString("") + (if (this.isAccepting) " \u2208 F" else "    ") + formulae.map{ form => form match {
      case q:Quantification => q.symbol + "Prot" + Pool.prototypes(q.phi.toString).id
      case automaton.phi    => "Prot" + automaton.id
      case automaton.phi.abstraction => "[Prot" + automaton.id + "]"
      case _                => form
    }}.mkString(": { ", " , ", " }")
   def printDetails          = Log.myprintln(details)
  
  /**
   * Override methods for atom comparison:
   * Two atoms are considered equal whenever they have the same string representation
   */
  override def equals(o: Any) = o match {
    case that: Atom => toString == that.toString
    case                _ => false
  }
  override def hashCode = toString.hashCode
}

/**
 * Helper for atom id generation & ordering for sorted sets
 */
object AtomNameGemerator { val indices = mutable.HashMap[Int,Int]() ; def getNext(prototypeIndex: Int): Int = { indices.get(prototypeIndex) match {case None => indices.put(prototypeIndex,1) ; return 0 case Some(x) => indices.put(prototypeIndex,x+1); return x } } }
object AtomOrdering extends Ordering[Atom] { def compare(a:Atom, b:Atom) = if (a.automaton.id == b.automaton.id) a.id compare b.id else a.automaton.id compare b.automaton.id }