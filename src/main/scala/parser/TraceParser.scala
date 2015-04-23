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

import datatype.Types._
import datatype._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical._

class TraceParser(struct: Structure) extends StandardTokenParsers with PackratParsers {

  lexical.delimiters +=("(", ")", "{", "}", ",", "-")

  def predParser: Parser[Elem] = elem("predicate", p => struct.uPreds(p.chars))

  val action: Parser[Action] = predParser ~ "(" ~ rep1sep("-".? ~ numericLit, ",") <~ ")" ^^ { case name ~ _ ~ args => (name.chars, args.toVector.map{ str => if (str._1.isEmpty) str._2 else "-" + str._2 })}
  val event : Parser[Event]  = "{" ~> repsep(action, ",") <~ "}" ^^ { case actions => actions.toSet}
  val trace : Parser[Trace]  = rep1sep(event, ",") ^^ { case events => Trace(events)}

  def parse(input: String): Option[Trace] = {
    val tokens = new lexical.Scanner(input)
    phrase(trace)(tokens) match {
      case Success(result, _) => Some(result)
      case f: NoSuccess => println("Incorrect trace syntax: " + f.msg); None
    }
  }
  def parseEvent(input: String): Option[Event] = {
    val tokens = new lexical.Scanner(input)
    phrase(event)(tokens) match {
      case Success(result, _) => Some(result)
      case f: NoSuccess => println("Incorrect event syntax: " + f.msg); None
    }
  }
}