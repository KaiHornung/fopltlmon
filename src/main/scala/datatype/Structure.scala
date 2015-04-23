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

/**
 * First-order structure
 * require: uPreds, iPreds, functs and consts must be disjoint
 */
class Structure {
  var uPreds: mutable.HashSet[String]                         = mutable.HashSet[String]()
  var iPreds: mutable.HashMap[String, Vector[Any] => Boolean] = mutable.HashMap[String, (Vector[Any] => Boolean)]()
  var functs: mutable.HashMap[String, Vector[Any] => Any]     = mutable.HashMap[String, Vector[Any] => Any]()
  var consts: mutable.HashMap[String, Any]                    = mutable.HashMap[String, Any]()

  def addUPredicate(name: String)                                         = if (isFreeName(name)) uPreds.add(name)
  def addIPredicate(name: String, interpretation: Vector[Any] => Boolean) = if (isFreeName(name)) iPreds.put(name, interpretation)
  def addFunction  (name: String, interpretation: Vector[Any] => Any)     = if (isFreeName(name)) functs.put(name, interpretation)
  def addConstant  (name: String, interpretation: Any)                    = if (isFreeName(name)) consts.put(name, interpretation)

  def isFreeName(name: String): Boolean = {
    if (!uPreds(name) && !iPreds.keySet(name) && !functs.keySet(name) && !consts.keySet(name)) {
      true
    } else {
      throw new Exception("Oops! Name " + name + " already declared.")
    }
  }
}
