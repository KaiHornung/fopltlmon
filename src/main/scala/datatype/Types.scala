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
package datatype

import scala.collection._
import monitor._

object Types {
  type Formulae    = SortedSet[Formula]
  
  type Assignment       = Map[String, Any]
  def initialAssignment = Map[String, Any]()
  
  type Obligations       = List[Obligation]
  def initialObligations = List[Obligation]()
  
  type Result      = Map[String, Int]
  
  type Event       = Set[Action]
  type Action      = (String, Vector[Any])
  
  type Valuation                     = Map[String, (Int,Int,Boolean)]
  def initialValuation(phi: Formula) = phi.parameters.map { case (par,upward) => (par -> (if (upward) (0,-1,false) else (0,Int.MaxValue,false))) }
    
  object UpdateSituation extends Enumeration {
    val BothPresent, ChildPresent, ParentPresent, NonePresent = Value
  }
  type UpdateSituation = UpdateSituation.Value
  type UpdateAction    = (UpdateSituation,Boolean)
}