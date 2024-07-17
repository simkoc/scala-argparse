package de.halcony.argparse.parsing

import de.halcony.argparse.ParsingResult

import scala.annotation.nowarn

abstract class ParsedArgument[T](name: String,
                                 description: String,
                                 @nowarn process: String => T)
    extends BaseArgument(name, description) {

  def parse(args: Iterable[String])(
      implicit result: ParsingResult): Iterable[String]

  def help(): String

}
/*
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

  private[argparse] override def parse(args: List[String])(
    implicit result: ParsingResult): List[String] = {
    var ret = args
    if (s"-$short" == args.head) {
      ret = args.tail
      result.addResult(name, SomeOptionalValue(ret.head, provided = true))
      ret = ret.tail
      parsed = true
    } else {
      long match {
        case "" =>
        case x =>
          if (s"--$x" == args.head) {
            ret = args.tail
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


object HelpFlag extends Flag("help", "h", "help", "prints this help message") {

  override private[argparse] def parse(args: List[String])(
    implicit result: ParsingResult): List[String] = {
    if ("-h" == args.head || "--help" == args.head) {
      throw new HelpException
    } else {
      args
    }
  }

}*/
