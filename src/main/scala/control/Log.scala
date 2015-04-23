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
package control

import monitor._
import datatype.Types._
import scala.collection._

object Log {
  var step                = 0
  
  def verbose(event: Event) = {
    if (Config.debug) myprintln("==========================================================================================")
    myprintln( ( padInt(step,5) :: padInt(event.size,2) :: padInt(Pool.prototypes.size,2) :: padInt(Pool.automata.size,6) :: padInt(Pool.automata.map(_.runs.size).foldLeft(0){ case (a,b) => a + b}, 6) ::
      padInt((Runtime.getRuntime.totalMemory() / 1000000).toInt,5) :: (SortedSet[String]() ++ (event.map { case (par,v) => par + "(" + v.mkString(",") +  ")" })).mkString(" , ")  :: Nil ).mkString("   ")
    )
    if (Config.debug) myprintln("==========================================================================================")
  }
  
  def init = { myprintln("#      |e|   #p   #aut    #runs  MEM     event") 
               myprintln("==========================================================================================") }
  
  def padInt(n: Int, length: Int) = n.toString().padTo(length, " ").mkString("")
    
  
  var outStream: java.io.PrintWriter = null
  def myprintln(s:String) = if (outStream != null) outStream.println(s) else println(s)
}

object Timer {
  private var startTime = 0l 
  
  val times   = mutable.Buffer[(String,Long)]()
  
  def start   = startTime = System.currentTimeMillis()
  def restart = start
  def elapsed = System.currentTimeMillis() - startTime
}