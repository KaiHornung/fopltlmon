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

import java.io.File
import scala.collection._
import scala.io.StdIn._
import scala.io.Source
import datatype._
import datatype.Types._
import parser._
import control._
import monitor._
import monitor.Util._
import java.io.FileNotFoundException
import scala.util.control.Breaks._

/**
 * Configuration Class for command line interface
 */
case class CLIConfig(formula: String = "", traceFile: File = null, interactive: Boolean = true, verboseFile: File = null, verbose: Boolean = false, outputFile: File = null)

object Main {
  
  val cliOptionParser = new scopt.OptionParser[CLIConfig]("fopltlmon") {
    head("fopltlmon", "0.1\n")
    arg[String] ("<formula>")                            action { (form, config) => config.copy(formula     = form)                      } text("input formula as string")
    opt[File]   ('t', "trace") valueName("<trace-file>") action { (file, config) => config.copy(traceFile   = file, interactive = false) } text("path of trace file")
    opt[File]   ('o', "out")     valueName("<out-file>") action { (file, config) => config.copy(outputFile  = file)                      } text("output file for result")
    opt[File]   ('v', "verbose") valueName("<out-file>") action { (file, config) => config.copy(verboseFile = file, verbose     = true ) } text("store number of automata/runs & memory usage after each step in file")
    opt[Unit]   ('w', "weak")                            action { (_   , config) => Config.weak  = true ; config                         } text("use weak acceptance for until and eventually")
    opt[Unit]   ("debug")      hidden()                  action { (_   , config) => Config.debug = true ; config.copy(verbose = false)   }
    help("help") text("prints this usage text")
  }
  
  
  val structure        = Util.getStandardStructure()
  val traceParser      = new TraceParser(structure)
  val formulaParser    = new FormulaParser(structure)
  var toplevelSMA :SMA = null
  
  def main(args: Array[String]): Unit = {
    try {
      cliOptionParser.parse(args,CLIConfig()) match {
        case Some(config) => {
          
          Timer.start 
          
          formulaParser.parse(config.formula) match { 
            case Some(formula) => {
              if (config.verboseFile != null) {
                Log.outStream = new java.io.PrintWriter(config.verboseFile,"UTF-8")
                try { Log.init ; run(config,formula) } finally { Log.outStream.close() }
              } else {
                run(config,formula) 
              }
            }
            case None => // formula is bad, error message will have been displayed
          }
        } 
        case None => // arguments are bad, error message will have been displayed
      }   
    } catch {
      case ex: FileNotFoundException => Log.myprintln("Error reading file: " + ex.getMessage)
    }
  }
  
  def run(config: CLIConfig, formula: Formula) = {
    if (Config.debug) {
      Log.myprintln("Formula:            " + formula.size + " - " + formula)
      Log.myprintln("Abstraction:        " + formula.abstraction)
      Log.myprintln("Closure:            " + formula.closure.size + " - " + formula.closure.mkString("{  ", "  ;  ", "  }"))
      Log.myprintln("Parameters:         " + formula.parameters.mkString("[  " , "  ,  ", "  ]"))
    }    
    
    if (config.outputFile != null) {
      Config.printResults      = true
      Config.resultPrintWriter = new java.io.PrintWriter(config.outputFile,"UTF-8")
        
      Config.printVariables  = SortedSet[String]() ++ formula.allParametrizedlVariables
      Config.printParameters = SortedSet[String]() ++ formula.parameters.keySet
      val firstLine = "\ntime of inst.  " + " | " + (Config.printVariables.map { _.padTo(12, " ").mkString("") }).mkString("") + " | " + (Config.printParameters.map { _.padTo(10, " ").mkString("") }).mkString("")
      Config.resultPrintWriter.println(firstLine)
      Config.resultPrintWriter.println("=".padTo(firstLine.length(), "=").mkString(""))
      
    }

    //Build all needed automaton prototypes
    buildPrototypes(formula)
        
    Timer.times.append(("Prototype Build Time",Timer.elapsed)) ; Timer.restart 
    
    toplevelSMA = new SMA(Pool.prototypes(formula.toString), initialAssignment)
    Pool.automata.add(toplevelSMA)
    
    if (config.interactive) {
      Log.myprintln("Entering interactive mode... Enter \"done\" to see result.")
      
      var break = false
      while (!break) {
        print("Enter event: ")
        val in = readLine()
        if (Config.debug) Log.myprintln("Read: " + in)
        if (in == "done") { break = true } else traceParser.parseEvent(in) match {
          case Some(event) => process(event, config)
          case None        => // event is bad, try again
        }
      }
    } else {
      breakable { for (line <- Source.fromFile(config.traceFile, "UTF-8").getLines()) {
        traceParser.parseEvent(line) match {
          case Some(event) => if (process(event, config)) break
          case _           => break
        }
      } }
    }
        
    println("\n")
    
    var res:Result = null
    
    Log.myprintln(toplevelSMA.result match {
      case None => "No result, automaton not accepting!"
      case Some(result) => res = result ;  "Result: " + res.mkString(" , ")
    })
    
    Timer.times.append(("Result Compution Time",Timer.elapsed)) ; Timer.restart 
    
    if (Config.printResults) {
      try {
        Pool.automata.filter(_.isAccepting).foreach { sma => sma.printDetailedResult }
        
        Config.resultPrintWriter.println("\n\n")
        
        Timer.times.foreach { case (title,time) => Config.resultPrintWriter.println(title.padTo(22, " ").mkString("") + ": " + time + " ms")}
        
        if (res != null) Config.resultPrintWriter.println("Result: " + res.mkString(" , ") + "\n")
      } finally {Config.resultPrintWriter.close()}
    }
  }
  
  def buildPrototypes(formula: Formula) {
    if (Pool.prototypes.get(formula.toString).isEmpty) {
      
      formula.closure.filter(_.isInstanceOf[Quantification]).foreach { 
        case q:Quantification => buildPrototypes(q.phi)
        case _                =>
      }
      
      Pool.prototypes.put(formula.toString, new SMAPrototype(formula,structure))
    }
  }
  
  def process(event: Event, config: CLIConfig): Boolean = {
    assert(Pool.spAutomata.isEmpty)
    Log.step = Log.step + 1
    
    print("\r" + Log.step)
    
    if (Config.debug)  Log.myprintln("\nProcessing event " + Log.step + ": " + { if (event.size > 0) (event.map { case (par,v) => par + "(" + v.mkString(",") +  ")" }).mkString(" , ") else "-" } + "\n================================================")
        
    Pool.automata.foreach { case sma => sma.process(event) }  //Spawns new Automata
    
    Pool.automata ++= Pool.spAutomata.values
   
    Pool.spAutomata.clear()
    
    Pool.automata.filter{ sma => sma.isTrapped || sma.isFinal }.foreach { Pool.automata.remove }  //Cleanup
        
    if (config.verbose) Log.verbose(event)
    if (Config.debug)   Pool.automata.foreach { automaton => Log.myprintln(automaton.details) }
    
    if (Pool.automata.size == 0) { println("\n") ; ( if(toplevelSMA.isAccepting) Log.myprintln("Accepting") else Log.myprintln("Violation detected!") ) ; return true }
    
    return false
  }
}

/**
 * Global configuration
 */
object Config{
  var debug                                  = false  //Print debugging output
  var weak                                   = false  //Use weak acceptance semantics
  var printResults                           = false  //Print sub-results to file
  var resultPrintWriter: java.io.PrintWriter = null   //PrintWriter for sub-results
  var printVariables   : SortedSet[String]   = null   //Variables &
  var printParameters  : SortedSet[String]   = null   //Parameters for sub-results
}

/**
 * Global Automata & AutomataPrototype Pool
 */
object Pool{
  val prototypes = mutable.Map[String,SMAPrototype]()              //All Automata Prototypes          FormulaString -> SMAPrototype
  val automata   = mutable.SortedSet.empty[SMA](SMAOrdering)       //All Automata
  val spAutomata = mutable.Map[(String,Assignment),SMA]()          //Spawned Automata in each step    FormulaString X Variable-Assignment  -> SMA
}