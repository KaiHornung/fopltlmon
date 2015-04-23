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

/**
 * Obligations are list of automata that need to be accepting. For existential quantification, one accepting automaton suffices, for universal quantification, all need to be accepting. 
 */
sealed abstract class Obligation(val subAutomata: List[SMA]) {
  override lazy val toString = symbol + (subAutomata.map { sma => sma.prototype.id + "." + sma.id }).mkString("(", ",",")");
  val symbol  : String
  
  def isFinal : Boolean 
  def result  : Result
  
  /**
   * Override methods for obligation comparison:
   * Two obligations are considered equal whenever they have the same string representation
   */
  override def equals(o: Any) = o match {
    case that: Obligation => toString == that.toString
    case                _ => false
  }
  override def hashCode = toString.hashCode
}

case class ForallObligation(override val subAutomata: List[SMA]) extends Obligation(subAutomata){
  val symbol  = "\u2200"
  
  def isFinal = subAutomata.forall(_.isFinal)
  def result  = subAutomata.tail.foldLeft(subAutomata.head.result.get) { case (result,sma) => Util.merge(result,sma.result.get,sma.prototype.phi) }  //Assumption that a result exists makes sense, since otherwise not accepting
}
case class ExistsObligation(override val subAutomata: List[SMA]) extends Obligation(subAutomata){
  val symbol  = "\u2203"
  
  def isFinal = subAutomata.exists(_.isFinal)
  def result  = Util.bestResult( (subAutomata.filter { _.isAccepting }).map { sma => sma.result.get } , subAutomata.head.prototype.phi)              //Get best result of the ones that are accepting
}

/**
 * Helper to convert instantiations to obligations by spawning automata
 */
object Obligation {
  def apply(instantiation: Instantiation, assignment: Assignment, initialEvent: Event) = { instantiation match {
    case ForallInstantiation(phi,assignmentUpdates) => ForallObligation( assignmentUpdates.map { update => Util.spawn(phi,Util.extendAssignment(assignment,update), initialEvent) })
    case ExistsInstantiation(phi,assignmentUpdates) => ExistsObligation( assignmentUpdates.map { update => Util.spawn(phi,Util.extendAssignment(assignment,update), initialEvent) })
  }
    
  }
}

/**
 * An instantiation maps arguments to instances
 */
sealed abstract class Instantiation (phi: Formula, instances: List[ (Vector[Term] , Vector[Any]) ])
case class ForallInstantiation (phi: Formula, instances: List[ (Vector[Term] , Vector[Any]) ]) extends Instantiation(phi, instances)
case class ExistsInstantiation (phi: Formula, instances: List[ (Vector[Term] , Vector[Any]) ]) extends Instantiation(phi, instances)