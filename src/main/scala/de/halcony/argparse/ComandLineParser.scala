package de.halcony.argparse

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}


abstract class CommandLineParser(val name : String,val description : String) {

  private[argparse] var parsed : Boolean = false

  private[argparse] def parse(args : List[String])(implicit result : ParsingResult) : List[String]

  def help() : String

}

case class Default[T](override val name : String, value : T, override val description : String = "") extends CommandLineParser(name, description) {

  override def parse(args: List[String])(implicit result : ParsingResult): List[String] = throw new RuntimeException("there is no parsing for default arguments")

  override def help(): String = throw new RuntimeException("there is no help for default arguments")

}

case class Positional(override val name : String, override val description : String = "") extends CommandLineParser(name, description) {

  //var value : String = ""

  private[argparse] override def parse(args: List[String])(implicit result : ParsingResult): List[String] = {
    //value = args.head
    result.addResult(name, args.head)
    parsed = true
    args.tail
  }

  override def help(): String = {
    s"[$name] $description"
  }
}

case class Optional(override val name : String, short : String, long : String = "", default : Option[String] = None, override val description : String = "") extends CommandLineParser(name, description) {

  //var value : String = default

  private[argparse] override def parse(args: List[String])(implicit result : ParsingResult): List[String] = {
    var ret = args
    if(s"-$short" == args.head) {
      ret = args.tail
      //value = Some(ret.head)
      result.addResult(name, Some(ret.head))
      ret =  ret.tail
      parsed = true
    } else {
      long match {
        case "" =>
        case x => if (s"--$x" == args.head) {
          ret = args.tail
          //value = Some(ret.head)
          result.addResult(name, Some(ret.head))
          ret = ret.tail
          parsed = true
        }
      }
    }
    ret
  }

  override def help(): String = {
    val flags : String = (short, long) match {
      case (_, "") => s"-$short"
      case (_, x) => s"-$short/--$x"
    }
    val deflt = default match {
      case Some(x) => s"(def:$x)"
      case None => ""
    }
    s"$flags <value> $description $deflt"
  }
}

case class Flag(override val name : String, short : String, long : String = "", override val description : String = "") extends CommandLineParser(name, description) {

  //var value : Boolean = false

  private[argparse] override def parse(args: List[String])(implicit result : ParsingResult): List[String] = {
    var ret = args
    if(s"-$short" == args.head) {
      result.addResult(name, true)
      parsed = true
      ret =  ret.tail
    } else {
      long match {
        case "" =>
        case x => if (s"--$x" == args.head) {
          result.addResult(name, true)
          parsed = true
          ret = ret.tail
        }
      }
    }
    ret
  }

  override def help(): String = {
    val flags : String = (short, long) match {
      case (_, "") => s"-$short"
      case (_, x) => s"-$short/--$x"
    }
    s"$flags $description"
  }
}

class ParsingResult() {

  private sealed trait Result

  private sealed case class ResultValue[T](value : T) extends Result

  private val results : collection.mutable.Map[String, Result] = collection.mutable.Map()

  private[argparse] def addResult[T](name : String, value : T) : Unit = {
    val res = ResultValue[T](value)
    results.addOne((name, res))
  }

  def get[T](name : String) : T = {
    results.get(name) match {
      case Some(x : ResultValue[T]) => x.value
    }
  }

  def getOrElse[T](name : String, otherwise : () => T) : T = {
    results.getOrElse(name, otherwise) match {
      case x : ResultValue[T] => x.value
    }
  }

}

case class Parser(override val name : String, override val description : String = "") extends CommandLineParser(name, description) {

  private val positionals : collection.mutable.ListBuffer[CommandLineParser] = new ListBuffer[CommandLineParser]
  private val optionals : collection.mutable.ListBuffer[CommandLineParser] = new ListBuffer[CommandLineParser]
  private val flags : collection.mutable.ListBuffer[CommandLineParser] = new ListBuffer[CommandLineParser]
  private val subparsers : collection.mutable.ListBuffer[CommandLineParser] = new ListBuffer[CommandLineParser]
  private val defaults : collection.mutable.ListBuffer[CommandLineParser] = new ListBuffer[CommandLineParser]

  private[argparse] var parentParsers : List[CommandLineParser] = Nil

  def addPositional(name : String, description : String = "") : Parser = {
    positionals.append(Positional(name, description))
    this
  }

  def addOptional(name : String, short : String, long : String = "", default : Option[String] = None, description : String = "") : Parser = {
    optionals.append(Optional(name, short, long, default, description))
    this
  }

  def addFlag(name : String, short : String, long : String = "", description : String = "") : Parser = {
    flags.append(Flag(name, short, long, description))
    this
  }

  def addSubparser(parser : Parser) : Parser = {
    parser.parentParsers = this :: parentParsers
    subparsers.append(parser)
    this
  }

  def addDefault[T](name : String, value : T, description : String = "") : Parser = {
    defaults.append(Default[T](name, value, description))
    this
  }

  def chosenSubparser() : (String,Parser) = {
    if(subparsers.isEmpty) {
      throw new RuntimeException("the chosen parser has no subparser")
    }
    subparsers.filter(_.parsed).head match {
      case x : Parser => (x.name, x)
    }
  }

  private def parsePositionals(args: List[String])(implicit result : ParsingResult = new ParsingResult()): List[String] = {
    var sargs = args
    for(positional <- positionals) {
      if(sargs.isEmpty) {
        throw new IntermediateParsingException("not all positional arguments were provided")
      }
      sargs = positional.parse(sargs)
    }
    sargs
  }

  private def parseOptionals(args: List[String])(implicit result : ParsingResult = new ParsingResult()): List[String] = {
    var sargs = args
    for(optional <- optionals ++ flags) {
      if(sargs.nonEmpty && !optional.parsed) {
        sargs = optional.parse(sargs)
      }
    }
    sargs
  }

  private def parseSubparser(args: List[String])(implicit result : ParsingResult = new ParsingResult()) : List[String] = {
    breakable {
      if(subparsers.nonEmpty) {
        for (subparser <- subparsers) {
          if (args.isEmpty) {
            throw new IntermediateParsingException("no more arguments and a subaction has still to be chosen")
          }
          if (subparser.name == args.head) {
            val remain = subparser.parse(args)
            if (remain.nonEmpty) {
              throw new ParsingException(s"there is junk at the end of the arguments $remain")
            }
            break()
          }
        }
        throw new IntermediateParsingException("no subaction was chosen")
      }
    }
    Nil
  }

  override def parse(args: List[String])(implicit result : ParsingResult = new ParsingResult()): List[String] = {
    try {
      assert((args.nonEmpty && args.head == this.name) || parentParsers.isEmpty)
      parsed = true
      defaults.foreach(default => result.addResult(default.name, default.asInstanceOf[Default[AnyRef]].value))
      optionals.foreach(optional => result.addResult(optional.name, optional.asInstanceOf[Optional].default))
      flags.foreach(flag => result.addResult(flag.name, false))
      parseSubparser(parseOptionals(parsePositionals(if(parentParsers.isEmpty) args else args.tail)))
    } catch {
      case x : IntermediateParsingException =>
        println(this.help())
        throw new ParsingException(x.getMessage)
      case _ : HelpException =>
        println(this.help())
        throw new ParsingException("The help flag was encountered")
    }
  }

  def parseArgv(argv : List[String]) : ParsingResult = {
    implicit val parsingResult: ParsingResult = new ParsingResult()
    parse(argv) match {
      case Nil =>
      case remains => throw new ParsingException(s"The input $argv was not completely parsed and $remains remains")
    }
    parsingResult
  }

  override def help(): String = {
    val msg = new StringBuilder
    msg ++= "usage: "
    if(parentParsers.nonEmpty) {
      msg ++= "... "
    }
    msg ++= name + " "
    for(positional <- positionals) {
      msg ++= s"[${positional.name}] "
    }
    msg ++= "{"
    msg ++= optionals.filter(_.isInstanceOf[Optional])
      .map(x => s"${x.asInstanceOf[Optional].short}").mkString(",")
    msg ++= ","
    msg ++= optionals.filter(_.isInstanceOf[Flag])
      .map(x => s"${x.asInstanceOf[Flag].short}").mkString(",")
    msg ++= ",-h,--help"
    msg ++= "} "
    msg ++= subparsers.map(_.name).mkString(" ")
    msg ++= "\n\n"
    msg ++= description + "\n\n          "
    msg ++= (positionals ++ optionals).map(_.help()).mkString("\n\n          ")
    msg ++= "\n\n          "
    msg ++= "-h/--help prints this help message"
    msg ++= "\n\n          "
    msg ++= subparsers.map(sp => s"${sp.name} ${sp.description}").mkString("\n\n          ")
    msg ++= "\n\n\n"
    msg.toString
    }
}

