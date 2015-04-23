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

import Types._
import scala.collection._
import scala.math._
import com.sun.tools.javac.processing.JavacProcessingEnvironment.ComputeAnnotationSet

/**
 * Formula class definitions
 */
sealed abstract class Formula {
  //Atomic Properties
  val size              : Int
  val depth             : Int
  
  val isParametric      : Boolean
  
  val needsParentheses  = false
  
  //Precomputed subformulae and closure
  lazy val abstraction: Formula  = if (isParametric) computeAbstraction else this
  lazy val closure    : Formulae = if (isParametric) subformulae ++ abstraction.closure else subformulae ++ (subformulae map {f => Negate(f)}) 
  val subformulae       : Formulae
  val atomicSubformulae : Formulae
  val allParametrizedlVariables : Set[String]
  val parameters        = if (isParametric) Map[String,Boolean]() else computeParameters  //Maps parameter to "is upward-monotone"
    
  def computeAbstraction: Formula
  def computeParameters : Map[String,Boolean]
}

object FormulaOrdering extends Ordering[Formula] {
  def compare(a:Formula, b:Formula) = if (a.size == b.size) (a.toString() compare b.toString()) else (a.size compare b.size)
}


/**
 * Atomic Formulae
 */
sealed abstract class Predicate(name: String, arguments: Vector[Term]) extends Formula {
  override lazy val toString = (if (name == "!=") "\u2260" else name) + "(" + arguments.mkString(", ") + ")"

  val size              = 1
  val depth             = 0
  val isParametric      = false
  
  val subformulae       = mutable.SortedSet.empty[Formula](FormulaOrdering) + this
  val atomicSubformulae = mutable.SortedSet.empty[Formula](FormulaOrdering) + this
  val allParametrizedlVariables = Set[String]()
  
  def computeAbstraction = this                   //DUMMY, never called
  def computeParameters  = Map[String,Boolean]()  //DUMMY, never called
  def interpret(struct: Structure, event: Event, v: Assignment): Boolean
}

case class InterpretedPredicate(name: String, arguments: Vector[Term]) extends Predicate(name, arguments) {  
  def interpret(struct: Structure, event: Event, v: Assignment) = struct.iPreds.get(name) match {
    case None    => false
    case Some(f) => f(arguments.map(_.interpret(struct, v)))
  }
}

case class UninterpretedPredicate(name: String, arguments: Vector[Term]) extends Predicate(name, arguments) {
  def interpret(struct: Structure, event: Event, v: Assignment) = if (event((this.name, arguments.map(_.interpret(struct, v).get.toString)))) true else false
}

case object True extends Formula {
  override lazy val toString = "\u22A4"
  
  val size              = 0
  val depth             = 0
  val isParametric      = false
 
  val subformulae       = mutable.SortedSet.empty[Formula](FormulaOrdering) + this
  val atomicSubformulae = mutable.SortedSet.empty[Formula](FormulaOrdering) + this
  val allParametrizedlVariables      = Set[String]()
  
  def computeAbstraction = this                   //DUMMY, never called
  def computeParameters  = Map[String,Boolean]()  //DUMMY, never called
}

case object False extends Formula {
  override lazy val toString = "\u22A5"

  val size              = 0
  val depth             = 0
  val isParametric      = false
  
  val subformulae       = mutable.SortedSet.empty[Formula](FormulaOrdering) + this
  val atomicSubformulae = mutable.SortedSet.empty[Formula](FormulaOrdering) + this
  val allParametrizedlVariables      = Set[String]()
  
  def computeAbstraction = this                   //DUMMY, never called
  def computeParameters  = Map[String,Boolean]()  //DUMMY, never called
}




/**
 * Unary Operators
 */
sealed abstract class UnaryOperator(phi: Formula) extends Formula {
  val symbol            : String
  override lazy val toString = symbol + (if (phi.needsParentheses) "(" + phi + ")" else phi)
  
  val size              = 1 + phi.size
  val depth             = phi.depth
  val isParametric      = phi.isParametric
  
  val subformulae       = phi.subformulae + this
  val atomicSubformulae = phi.atomicSubformulae
  val allParametrizedlVariables = phi.allParametrizedlVariables
  
  def computeParameters = phi.parameters
}

case class Not(phi: Formula) extends UnaryOperator(phi) {
  val symbol = "\u00AC"
  
  def computeAbstraction = Negate(phi.abstraction)
}

case class Next(phi: Formula) extends UnaryOperator(phi) {
  val symbol = "\u25EF"
  
  def computeAbstraction = Next(phi.abstraction)
}

// Quantifiers
sealed abstract class Quantification(val guard: UninterpretedPredicate, val phi: Formula) extends UnaryOperator(phi) {
  require(guard.arguments.forall(_.isInstanceOf[Var]))
  
  override val depth             = phi.depth + 1
  override val subformulae       = mutable.SortedSet.empty[Formula](FormulaOrdering) + this
  override val atomicSubformulae = mutable.SortedSet.empty[Formula](FormulaOrdering)
  override val allParametrizedlVariables = if (phi.isParametric) phi.allParametrizedlVariables ++ guard.arguments.map { case Var(x) => x case _ => "" } else Set[String]()
}

case class Forall(override val guard: UninterpretedPredicate, override val phi: Formula) extends Quantification(guard, phi) {
  val symbol = "\u2200" + guard.arguments.mkString(",") + ":" + guard.name + "."
  
  def computeAbstraction = Forall(guard,phi.abstraction)
}

case class Exists(override val guard: UninterpretedPredicate, override val phi: Formula) extends Quantification(guard, phi) { 
  val symbol = "\u2203" + guard.arguments.mkString(",") + ":" + guard.name + "."
  
  def computeAbstraction = Exists(guard,phi.abstraction)
}

// Parametric Operators
sealed abstract class ParametricOperator(parameter: String, phi: Formula) extends UnaryOperator(phi) { 
  override val needsParentheses  = true
  override val isParametric      = true
}

case class PEventually(parameter: String, phi: Formula) extends ParametricOperator(parameter, phi) {
  val symbol                   = "\u2662" + "\u2264" + parameter + (if (phi.needsParentheses) "" else " ")
  
  def computeAbstraction  = Eventually(phi.abstraction)
  
  override def computeParameters = phi.parameters + (parameter -> true)
}

case class PGlobally(parameter: String, phi: Formula) extends ParametricOperator(parameter, phi) {
  val symbol                   = "\u25A2" + "\u2264" + parameter + (if (phi.needsParentheses) "" else " ")
  
  def computeAbstraction = phi.abstraction
  
  override def computeParameters = phi.parameters + (parameter -> false)
}


/**
 * Binary Operators
 */
sealed abstract class BinaryOperator(phi: Formula, psi: Formula) extends Formula {
  override val needsParentheses  = true
  val symbol            : String
  override lazy val toString = { (if (phi.needsParentheses) "(" + phi + ")" else phi) + symbol + (if (psi.needsParentheses) "(" + psi + ")" else psi) }
  
  val size              = 1 + phi.size + psi.size
  val depth             = min(phi.depth, psi.depth) 

  val isParametric      = phi.isParametric || psi.isParametric
  
  val subformulae       = (phi.subformulae ++ psi.subformulae) + this
  val atomicSubformulae = (phi.atomicSubformulae ++ psi.atomicSubformulae)
  val allParametrizedlVariables = (phi.allParametrizedlVariables ++ psi.allParametrizedlVariables)
  
  def computeParameters = phi.parameters ++ psi.parameters
}

case class Conjunction(phi: Formula, psi: Formula) extends BinaryOperator(phi, psi) {
  val symbol = " \u2227 "
  
  def computeAbstraction = Conjunction(phi.abstraction, psi.abstraction)
}

case class Disjunction(phi: Formula, psi: Formula) extends BinaryOperator(phi, psi) {
  val symbol = " \u2228 "
  
  def computeAbstraction = Disjunction(phi.abstraction, psi.abstraction)
}

case class Until(phi: Formula, psi: Formula) extends BinaryOperator(phi, psi) {
  val symbol = " U "
  override lazy val toString = if (phi == True) "\u2662" + (if (psi.needsParentheses) "(" + psi + ")" else " " + psi) else (if (phi.needsParentheses) "(" + phi + ")" else phi) + symbol + (if (psi.needsParentheses) "(" + psi + ")" else psi)
  
  def computeAbstraction = Until(phi.abstraction, psi.abstraction)
}

case class Release(phi: Formula, psi: Formula) extends BinaryOperator(phi, psi) {
  val symbol = " R "
  override lazy val toString = if (phi == False) "\u25A2" + (if (psi.needsParentheses) "(" + psi + ")" else " " + psi) else (if (phi.needsParentheses) "(" + phi + ")" else phi) + symbol + (if (psi.needsParentheses) "(" + psi + ")" else psi)
    
  def computeAbstraction = Release(phi.abstraction, psi.abstraction)
}


/**
 * Derived operators
 */
object Implication {
  def apply(phi: Formula, psi: Formula) = Disjunction(Negate(phi), psi)
  def unapply(formula: Formula)         = formula match { case Disjunction(phi, psi) => Some(Negate(phi), psi) case _ => None }
}

object Globally {
  def apply(phi: Formula, parameter: String = "", greater: Boolean = false) = (parameter,greater) match {
    case ("", _)     => Release(False, phi)
    case (s , false) => PGlobally(s, phi)
    case (s , true)  => PEventually(s, Release(False, Next(phi)))
  }
  def unapply(formula: Formula) = formula match { 
    case Release(False,phi:Formula)                        => Some(phi, "", false)
    case PGlobally(parameter,phi:Formula)                  => Some(phi, parameter, false)
    case PEventually(s, Release(False, Next(phi:Formula))) => Some(phi, s, true)
    case _                                                 => None
  }
}

object Eventually {
  def apply(phi: Formula, parameter: String = "", greater: Boolean = false) = (parameter,greater) match {
    case ("", _)     => Until(True, phi)
    case (s , false) => PEventually(s, phi)
    case (s , true)  => PGlobally(s, Until(True, Next(phi)))
  }
  def unapply(formula: Formula) = formula match { 
    case Until(True,phi:Formula)                      => Some(phi, "", false)
    case PEventually(parameter,phi:Formula)           => Some(phi, parameter, false)
    case PGlobally(s, Until(True, Next(phi:Formula))) => Some(phi, s, true)
    case _                                            => None
  }
}

object PUntil {
  def apply(phi: Formula, psi: Formula, parameter: String, greater: Boolean = false):Formula = greater match {
    case true  => PGlobally(parameter, Conjunction(phi, Next(Until(phi, psi)))) 
    case false => Conjunction(Until(phi, psi), PEventually(parameter, psi)) 
  } 
}

object PRelease {
  def apply(phi: Formula, psi: Formula, parameter: String, greater: Boolean = false):Formula = greater match {
    case true  => PEventually(parameter, Disjunction(phi, Next(Release(phi, psi)))) 
    case false => Conjunction(Disjunction(phi,PGlobally(parameter, Negate(phi.abstraction))),Release(phi, psi))//Disjunction(Release(phi, psi), PGlobally(parameter,phi)) 
  } 
}

/**
 * Negation function to maintain PNF
 */
object Negate {
  def apply(phi: Formula): Formula = phi match {
    case True                 => False
    case False                => True
    // eliminate double negations
    case Not(phi)              => phi
    
    case Next(phi)             => Next(Negate(phi))
    
    case Forall   (guard, phi) => Exists(guard, Negate(phi))
    case Exists   (guard, phi) => Forall(guard, Negate(phi))
    
    case PEventually(par, phi) => PGlobally  (par, Negate(phi))
    case PGlobally  (par, phi) => PEventually(par, Negate(phi))
    
    case Conjunction(phi, psi) => Disjunction(Negate(phi), Negate(psi))
    case Disjunction(phi, psi) => Conjunction(Negate(phi), Negate(psi))
    
    case Until      (phi, psi) => Release(Negate(phi), Negate(psi))
    case Release    (phi, psi) => Until  (Negate(phi), Negate(psi))
    
    case p: Predicate          => Not(p)
  }

  def unapply(phi: Formula): Option[Formula] = Some(Negate(phi))
}
