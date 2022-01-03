package de.halcony.argparse

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

abstract class CommandLineParser(val name: String, val description: String) {

  private[argparse] var parsed: Boolean = false

  private[argparse] def parse(args: List[String])(
      implicit result: ParsingResult): List[String]

  def help(): String

}

case class Default[T](override val name: String,
                      value: T,
                      override val description: String = "")
    extends CommandLineParser(name, description) {

  override def parse(args: List[String])(
      implicit result: ParsingResult): List[String] =
    throw new RuntimeException("there is no parsing for default arguments")

  override def help(): String =
    throw new RuntimeException("there is no help for default arguments")

}

case class Positional(override val name: String,
                      override val description: String = "")
    extends CommandLineParser(name, description) {

  private[argparse] override def parse(args: List[String])(
      implicit result: ParsingResult): List[String] = {
    result.addResult(name, PositionalValue(args.head))
    parsed = true
    args.tail
  }

  override def help(): String = {
    s"[$name] $description"
  }
}

case class Optional(override val name: String,
                    short: String,
                    long: String = "",
                    default: Option[String] = None,
                    override val description: String = "")
    extends CommandLineParser(name, description) {

  //var value : String = default

  private[argparse] override def parse(args: List[String])(
      implicit result: ParsingResult): List[String] = {
    var ret = args
    if (s"-$short" == args.head) {
      ret = args.tail
      //value = Some(ret.head)
      result.addResult(name, SomeOptionalValue(ret.head, provided = true))
      ret = ret.tail
      parsed = true
    } else {
      long match {
        case "" =>
        case x =>
          if (s"--$x" == args.head) {
            ret = args.tail
            //value = Some(ret.head)
            result.addResult(name, SomeOptionalValue(ret.head, provided = true))
            ret = ret.tail
            parsed = true
          }
      }
    }
    ret
  }

  override def help(): String = {
    val flags: String = (short, long) match {
      case (_, "") => s"-$short"
      case (_, x)  => s"-$short/--$x"
    }
    val deflt = default match {
      case Some(x) => s"(def:$x)"
      case None    => ""
    }
    s"$flags <value> $description $deflt"
  }
}

case class Flag(override val name: String,
                short: String,
                long: String = "",
                override val description: String = "")
    extends CommandLineParser(name, description) {

  //var value : Boolean = false

  private[argparse] override def parse(args: List[String])(
      implicit result: ParsingResult): List[String] = {
    var ret = args
    if (s"-$short" == args.head) {
      result.addResult(name, FlagValue(value = true, provided = true))
      parsed = true
      ret = ret.tail
    } else {
      long match {
        case "" =>
        case x =>
          if (s"--$x" == args.head) {
            result.addResult(name, FlagValue(value = true, provided = true))
            parsed = true
            ret = ret.tail
          }
      }
    }
    ret
  }

  override def help(): String = {
    val flags: String = (short, long) match {
      case (_, "") => s"-$short"
      case (_, x)  => s"-$short/--$x"
    }
    s"$flags $description"
  }
}

object HelpFlag extends Flag("help", "h", "help", "prints this help message") {

  override private[argparse] def parse(args: List[String])(
      implicit result: ParsingResult): List[String] = {
    if ("-h" == args.head || "--help" == args.head) {
      throw new HelpException
    } else {
      args
    }
  }

}

case class Parser(override val name: String,
                  override val description: String = "")
    extends CommandLineParser(name, description) {

  private val positionals: collection.mutable.ListBuffer[CommandLineParser] =
    new ListBuffer[CommandLineParser]
  private val optionals: collection.mutable.ListBuffer[CommandLineParser] =
    new ListBuffer[CommandLineParser]
  private val flags: collection.mutable.ListBuffer[CommandLineParser] =
    new ListBuffer[CommandLineParser]
  flags.addOne(HelpFlag)
  private val subparsers: collection.mutable.ListBuffer[CommandLineParser] =
    new ListBuffer[CommandLineParser]
  private val defaults: collection.mutable.ListBuffer[CommandLineParser] =
    new ListBuffer[CommandLineParser]

  private[argparse] var parentParsers: List[CommandLineParser] = Nil

  def addPositional(name: String, description: String = ""): Parser = {
    positionals.append(Positional(name, description))
    this
  }

  def addOptional(name: String,
                  short: String,
                  long: String = "",
                  default: Option[String] = None,
                  description: String = ""): Parser = {
    optionals.append(Optional(name, short, long, default, description))
    this
  }

  def addFlag(name: String,
              short: String,
              long: String = "",
              description: String = ""): Parser = {
    flags.append(Flag(name, short, long, description))
    this
  }

  def addSubparser(parser: Parser): Parser = {
    parser.parentParsers = this :: parentParsers
    subparsers.append(parser)
    this
  }

  def addDefault[T](name: String,
                    value: T,
                    description: String = ""): Parser = {
    defaults.append(Default[T](name, value, description))
    this
  }

  def chosenSubparser(): (String, Parser) = {
    if (subparsers.isEmpty) {
      throw new RuntimeException("the chosen parser has no subparser")
    }
    subparsers.filter(_.parsed).head match {
      case x: Parser => (x.name, x)
    }
  }

  private def parsePositionals(args: List[String])(
      implicit result: ParsingResult): List[String] = {
    var sargs = args
    for (positional <- positionals) {
      if (sargs.isEmpty) {
        throw new IntermediateParsingException(
          "not all positional arguments were provided")
      }
      sargs = positional.parse(sargs)
    }
    sargs
  }

  private def parseOptionals(args: List[String])(
      implicit result: ParsingResult): List[String] = {
    var sargs = args
    var change = true
    while (change) {
      change = false
      for (optional <- optionals ++ flags) {
        if (sargs.nonEmpty && !optional.parsed) {
          val newArgs = optional.parse(sargs)
          if (newArgs.length < sargs.length) {
            change = true
            sargs = newArgs
          }
        }
      }
    }
    sargs
  }

  private def parseSubparser(args: List[String])(
      implicit result: ParsingResult): List[String] = {
    breakable {
      if (subparsers.nonEmpty) {
        for (subparser <- subparsers) {
          if (args.isEmpty) {
            throw new IntermediateParsingException(
              "no more arguments and a subaction has still to be chosen")
          }
          if (subparser.name == args.head) {
            val remain = subparser.parse(args)
            if (remain.nonEmpty) {
              throw new ParsingException(
                s"there is junk at the end of the arguments $remain")
            }
            break()
          }
        }
        throw new IntermediateParsingException("no subaction was chosen")
      }
    }
    Nil
  }

  override private[argparse] def parse(args: List[String])(
      implicit result: ParsingResult = new ParsingResult()): List[String] = {
    try {
      assert((args.nonEmpty && args.head == this.name) || parentParsers.isEmpty)
      parsed = true
      defaults.foreach(
        default =>
          result.addResult(
            default.name,
            DefaultValue(default.asInstanceOf[Default[AnyRef]].value)))
      optionals.foreach(
        optional =>
          result.addResult(optional.name,
                           optional.asInstanceOf[Optional].default match {
                             case Some(value) =>
                               SomeOptionalValue(value, provided = false)
                             case None => NoneOptionalValue()
                           }))
      flags.foreach(flag =>
        result.addResult(flag.name, FlagValue(value = false, provided = false)))
      parseSubparser(
        parseOptionals(
          parsePositionals(if (parentParsers.isEmpty) args else args.tail)))
    } catch {
      case x: IntermediateParsingException =>
        println(x.getMessage)
        println()
        println(this.help())
        throw new ParsingException(x.getMessage)
      case _: HelpException =>
        println(this.help())
        throw new ParsingException("The help flag was encountered")
    }
  }

  def parse(argv: Array[String]): ParsingResult = {
    implicit val parsingResult: ParsingResult = new ParsingResult()
    parse(argv.toList) match {
      case Nil =>
      case remains =>
        throw new ParsingException(
          s"The input ${argv.mkString("Array(", ", ", ")")} was not completely parsed and $remains remains")
    }
    parsingResult
  }

  override def help(): String = {
    val msg = new StringBuilder
    msg ++= "usage: "
    if (parentParsers.nonEmpty) {
      msg ++= "... "
    }
    msg ++= name + " "
    for (positional <- positionals) {
      msg ++= s"[${positional.name}] "
    }
    msg ++= "{"
    msg ++= optionals
      .filter(_.isInstanceOf[Optional])
      .map(x => s"-${x.asInstanceOf[Optional].short}")
      .mkString(",")
    if (optionals.exists(_.isInstanceOf[Optional])) {
      msg ++= ","
    }
    msg ++= flags
      .filter(_.isInstanceOf[Flag])
      .map(x => s"-${x.asInstanceOf[Flag].short}")
      .mkString(",")
    msg ++= "}"
    if (subparsers.nonEmpty) {
      msg ++= " "
    }
    msg ++= subparsers.map(_.name).mkString(" ")
    msg ++= "\n\n"
    msg ++= description + "\n\n"
    msg ++= (positionals ++ optionals ++ flags)
      .map(x => s"          ${x.help()}")
      .mkString("\n\n")
    if ((positionals ++ optionals ++ flags).nonEmpty) {
      msg ++= "\n\n"
    }
    msg ++= subparsers
      .map(sp => s"          ${sp.name} ${sp.description}")
      .mkString("\n\n")
    if (subparsers.nonEmpty) {
      msg ++= "\n\n"
    }
    msg ++= "\n"
    msg.toString
  }
}
