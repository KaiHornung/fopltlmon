/*******************************************************************************
 * This is part of fopltlmon (https://github.com/kaihornung/fopltlmon).
 *  
 *  Copyright (c) 2015 by Kai Hornung <development@khornung.com>
 *  Partially taken from ltlfo2mon (https://github.com/jckuester/ltlfo2mon)
 *    Copyright (c) 2013 by Jan-Christoph Kuester <kuester@sdf.org>
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
package parser

import datatype._
import control.Config._
import control._
import scala.collection._
import scala.util.parsing.combinator.syntactical._

class FormulaParser(struct: Structure) extends StandardTokenParsers with OperatorPrecedenceParsers {
  var variables : mutable.Set[String] = null
  var parameters: mutable.Set[String] = null
  
  lexical.delimiters += ("~", "&&", "||", "->",             //Boolean operators
                         "()", "[]", "<>",                  //Temporal operators
                         "+", "-", "*",                     //special function symbols
                         "=", "!=", "<=", "=>", "<", ">",   //special predicate symbols, Parameter bounds
                         ":", ".", "(", ")", ",")           //Other delimiters
                         
  lexical.reserved   += ( "U", "R", "X", "A", "E", "G", "F", "true", "false")
      
  lazy val literal  = literalParser ^^ { case v => Lit(v.chars.toInt)}
  lazy val variable = varNameParser ^^ { case v => Var(v.chars)}
  lazy val const    = conNameParser ^^ { case c => Con(c.chars)} 
  lazy val funct    = funNameParser ~ "(" ~ terms <~ ")" ^^ { case f ~ _ ~ vector => Fun(f.chars, vector)}
  
  lazy val terms: Parser[Vector[Term]] = rep1sep(literal | variable | const | funct, ",") ^^ { case list => list.toVector }
  val args                             = ("(" ~> terms <~ ")").? ^^ { case Some(terms) => terms case None => Vector[Term]() }
  lazy val vars                        = "(" ~> rep1sep(newVarParser, ",") <~ ")" ^^ { case list => variables ++= list.map(_.chars).toSet; list.map(x => Var(x.chars)).toVector } | newVarParser ^^ { case x => variables  += x.chars; Vector(Var(x.chars)) }
  lazy val pars                        = ("<="|">") ~ newParParser ^^ { case b ~ x => parameters += x.chars; (b , x.chars) }

  lazy val newVarParser: Parser[Elem] = elem("forallVar", v => struct.isFreeName(v.chars))
  lazy val newParParser: Parser[Elem] = elem("parameter", v => struct.isFreeName(v.chars))
  
  lazy val uPred = uPredNameParser ~ args ^^ { case name ~ attrs => UninterpretedPredicate(name.chars, attrs) }
  lazy val iPred = iPredNameParser ~ args ^^ { case name ~ attrs => InterpretedPredicate  (name.chars, attrs) } 
  val top    = "true"  ^^ {case _ => True}
  val bottom = "false" ^^ {case _ => False}
  val forall = "A" ~> vars ~ ":" ~ uPredNameParser <~"." ^^ { case vector ~ _ ~ uPredName =>  UninterpretedPredicate(uPredName.chars, vector)}
  val exists = "E" ~> vars ~ ":" ~ uPredNameParser <~"." ^^ { case vector ~ _ ~ uPredName =>  UninterpretedPredicate(uPredName.chars, vector)}
  
  val eventually = ("F"|"<>") ~ pars.? ^^ { case s ~ None => None case s ~ Some((b,p)) => Some(p,b == ">") }
  val globally   = ("G"|"[]") ~ pars.? ^^ { case s ~ None => None case s ~ Some((b,p)) => Some(p,b == ">") }
  val until      = "U" ~ pars.? ^^ { case s ~ None => None case s ~ Some((b,p)) => Some(p,b == ">") }
  val release    = "R" ~ pars.? ^^ { case s ~ None => None case s ~ Some((b,p)) => Some(p,b == ">") }
  
  def formula:Parser[Formula] = operators[Any,Formula](
      Prefix(100)("~")          { (_, phi)   => Negate(phi) },
      Prefix(100)("X"|"()")     { (_, phi)   => Next(phi) },
      Prefix(100)(eventually)   { (popt,phi) => popt match { case None => Eventually(phi) case Some((p,g)) => Eventually(phi,p,g) } },
      Prefix(100)(globally)     { (popt,phi) => popt match { case None => Globally  (phi) case Some((p,g)) => Globally  (phi,p,g) } },
      Infix(200, 200-1)(release){ (popt, phi, psi) => popt match { case None => Release(phi,psi) case Some((p,g)) => PRelease(phi,psi,p,g) } },
      Infix(300, 300-1)(until)  { (popt, phi, psi) => popt match { case None => Until  (phi,psi) case Some((p,g)) => PUntil  (phi,psi,p,g) } },
      Infix(400-1, 400)("&&")   { (_, phi, psi) => Conjunction(phi, psi) },
      Infix(500-1, 500)("||")   { (_, phi, psi) => Disjunction(phi, psi) },
      Infix(600-1, 600)("->")   { (_, phi, psi) => Implication(phi, psi) },
      Prefix(800)(forall)       { (uPred,phi) => Forall(uPred, phi) },
      Prefix(800)(exists)       { (uPred,phi) => Exists(uPred, phi) }
      ) ( "(" ~> formula <~ ")" | top | bottom | uPred | iPred )
    
  def literalParser  : Parser[Elem] = elem("literal"   , s => s.chars forall Character.isDigit)
  def varNameParser  : Parser[Elem] = elem("variable"  , v => variables(v.chars))
  def parNameParser  : Parser[Elem] = elem("parameter" , p => parameters(p.chars))
  def conNameParser  : Parser[Elem] = elem("constant"  , c => struct.consts.keySet(c.chars))
  def funNameParser  : Parser[Elem] = elem("function"  , f => struct.functs.keySet(f.chars))
  def uPredNameParser: Parser[Elem] = elem("uPredicate", p => struct.uPreds(p.chars))
  def iPredNameParser: Parser[Elem] = elem("iPredicate", r => struct.iPreds.keySet(r.chars))
   
  /**
   * returns a formula in PNF
   */
  def parse(input: String): Option[Formula] = {
    
    variables  = mutable.Set[String]()
    parameters = mutable.Set[String]()
    
    val tokens = new lexical.Scanner(input)
    phrase(formula)(tokens) match {
      case Success(result, _) => Some(result)     
      case f: NoSuccess       => Log.myprintln("Incorrect formula syntax: " + f.msg); None     
    }
  }
}