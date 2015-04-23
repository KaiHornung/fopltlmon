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

import datatype.Types._

sealed trait Term { 
  /**
   * Interprets a term under the given Assignment
   * @return: None if an error occurs or variables are not bound
   */
  def interpret(s: Structure, v: Assignment): Option[Any]
}

/**
 * Term class definitions
 */

// variable
case class Var(name: String) extends Term {
  override def toString = name
  
  def interpret(s: Structure, v: Assignment) = v.get(name)
}

// integer literal
case class Lit(value: Any) extends Term {
  override def toString =  value.toString
  
  def interpret(s: Structure, v: Assignment) = Some(value)
}

// constant symbol
case class Con(name: String) extends Term {
  override def toString = name
  
  def interpret(s: Structure, v: Assignment) = s.consts.get(name)
}

// function Application
case class Fun(name: String, arguments: Vector[Term]) extends Term{
  override def toString = name + "(" + arguments + ")" 
  
  def interpret(s: Structure, v: Assignment) = (name, arguments.map(_.interpret(s, v))) match {
    case ("+", Vector(Some(a: Int),    Some(b: Int)))    => Some(a+b)
    case ("-", Vector(Some(a: Int),    Some(b: Int)))    => Some(a-b)
    case ("*", Vector(Some(a: Int),    Some(b: Int)))    => Some(a*b)
    case ("^", Vector(Some(a: String), Some(b: String))) => Some(a+b)
    case (oth, args)                                     => s.functs.get(name) match {
                                                                case None      => None
                                                                case Some(fun) => (fun(args)) match { case None => None case res => Some(res) }   
                                                                
                                                                //every function needs to be of type Vector[Option[Any]] -> Any
                                                            }
  }
}