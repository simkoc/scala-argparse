package de.halcony.argparse

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}


private abstract class CommandLineParser(val name : String,val description : String) {

  private[argparse] var parsed : Boolean = false

  def parse(args : List[String]) : List[String]

  def help() : String

}


private case class Positional(override val name : String, override val description : String = "") extends CommandLineParser(name, description) {

  var value : String = ""

  override def parse(args: List[String]): List[String] = {
    value = args.head
    parsed = true
    args.tail
  }

  override def help(): String = {
    s"[$name] $description"
  }
}

private case class Optional(override val name : String, short : String, long : String = "", default : Option[String] = None, override val description : String = "") extends CommandLineParser(name, description) {

  var value : Option[String] = default

  override def parse(args: List[String]): List[String] = {
    var ret = args
    if(s"-$short" == args.head) {
      ret = args.tail
      value = Some(ret.head)
      ret =  ret.tail
      parsed = true
    } else {
      long match {
        case "" =>
        case x => if (s"--$x" == args.head) {
          ret = args.tail
          value = Some(ret.head)
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

private case class Flag(override val name : String, short : String, long : String = "", override val description : String = "") extends CommandLineParser(name, description) {

  var value : Boolean = false

  override def parse(args: List[String]): List[String] = {
    var ret = args
    if(s"-$short" == args.head) {
      value = true
      parsed = true
      ret =  ret.tail
    } else {
      long match {
        case "" =>
        case x => if (s"--$x" == args.head) {
          value = true
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

private case class Parser(override val name : String, override val description : String = "") extends CommandLineParser(name, description) {

  private val positionals : collection.mutable.ListBuffer[CommandLineParser] = new ListBuffer[CommandLineParser]
  private val optionals : collection.mutable.ListBuffer[CommandLineParser] = new ListBuffer[CommandLineParser]
  private val subparsers : collection.mutable.ListBuffer[CommandLineParser] = new ListBuffer[CommandLineParser]
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
    optionals.append(Flag(name, short, long, description))
    this
  }

  def addSubparser(parser : Parser) : Parser = {
    parser.parentParsers = this :: parentParsers
    subparsers.append(parser)
    this
  }

  def get[T](name : String) : T = {
    var ret : Option[T] = None
    breakable {
      for(clp <- positionals ++ optionals ++ subparsers) {
        if(clp.name == name) {
          clp match {
            case x : T =>
              ret = Some(x)
              break
            case _ =>
          }
        }
      }
    }
    ret.getOrElse(throw new RuntimeException(s"there is no parser of name $name"))
  }

  override def parse(args: List[String]): List[String] = {
    var sargs = args
    var remaining : List[String] = Nil
    if(args.nonEmpty && (parentParsers.isEmpty || args.head == name)) {
      parsed = true
      if (parentParsers.nonEmpty) {
        sargs = args.tail
      }
      for (positional <- positionals) {
        if (!positional.parsed) {
          remaining = positional.parse(sargs)
          if(remaining.length < sargs.length) {
            remaining = this.parse(remaining)
          }
        }
      }
      assert(remaining.length == sargs.length || remaining.isEmpty,
        s"either no positional is parsed or parsing is done but $args remain")
      for (optional <- optionals) {
        if (!optional.parsed) {
          remaining = optional.parse(sargs)
          if(remaining.length < sargs.length) {
            remaining = this.parse(remaining)
          }
        }
      }
      assert(remaining.length == sargs.length || remaining.isEmpty,
        s"either no optional is parsed or parsing is done but $args remain")
      breakable {
        for (subparser <- subparsers) {
          if (subparser.name == sargs.head) {
            remaining = subparser.parse(sargs)
            break()
          }
        }
      }
    }
    assert(remaining.isEmpty, s"after applying all parsers $remaining still remains")
    assert(positionals.map(_.parsed).reduceOption(_ && _).getOrElse(true), "at least one positional was not satisfied")
    assert(subparsers.map(_.parsed).reduceOption(_ || _).getOrElse(true), "no subparser was triggered")
    Nil
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
    msg ++= "} "
    msg ++= subparsers.map(_.name).mkString(" ")
    msg ++= "\n\n"
    msg ++= description + "\n\n"
    msg ++= (positionals ++ optionals).map(_.help()).mkString("\n")
    msg ++= "\n"
    msg ++= subparsers.map(sp => s"${sp.name} ${sp.description}").mkString("\n")
    msg.toString
    }
}

