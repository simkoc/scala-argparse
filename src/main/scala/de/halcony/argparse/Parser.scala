package de.halcony.argparse

import de.halcony.argparse.parsing.{
  BaseArgument,
  DefaultArgument,
  FlagArgument,
  HelpException,
  HelpFlag,
  OptionalArgument,
  ParsedArgument,
  PositionalArgument
}

import scala.annotation.nowarn
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

case class Parser(override val name: String,
                  override val description: String = "")
    extends ParsedArgument[AnyVal](
      name,
      description,
      _ => throw new RuntimeException("no processing for a parser")) {

  private var parent: Option[Parser] = None
  def setParent(parent: Parser): Parser = {
    this.parent = Some(parent)
    this
  }

  private val defaults: collection.mutable.ListBuffer[BaseArgument] =
    new ListBuffer[BaseArgument]

  /** Add a default argument to the current parser
    *
    * the deepest used parser's default argument will take precedent
    *
    * @param name the name of the default argument
    * @param value the value the default argument is supposed to take
    * @tparam T the type of the value of the default argument
    * @return the parser itself to chain commands
    */
  @nowarn
  def addDefault[T](name: String, value: T): Parser = {
    defaults.append(DefaultArgument[T](name, value))
    this
  }

  private val positionals
    : collection.mutable.ListBuffer[PositionalArgument[_]] =
    new ListBuffer[PositionalArgument[_]]()

  /** add a positional argument
    *
    * @param name the name of the positional argument
    * @return the parser itself to chain commands
    */
  def addPositional[T](name: String, process: String => T)(
      implicit tag: ClassTag[T]): Parser = {
    positionals.append(PositionalArgument[T](name, description, process))
    this
  }

  /** add a positional argument
    *
    * @param name the name of the positional argument
    * @param process the function to process the cmd line string to the argument type
    * @param description the description of the argument
    * @tparam T the type of the finally parsed positional argument after parsing
    * @return the parser itself to chain commands
    */
  def addPositional[T](name: String, process: String => T, description: String)(
      implicit tag: ClassTag[T]): Parser = {
    val toBeAdded = PositionalArgument[T](name, description, process)
    this.positionals += toBeAdded
    this
  }

  private val flags: collection.mutable.ListBuffer[FlagArgument] =
    new ListBuffer[FlagArgument]

  flags.append(HelpFlag)

  /** add a flag
    *
    * @param name the name of the flag
    * @param short the short name of the flag
    * @param long the long name of the flag
    * @param description the description of what the flag affects
    * @return the parser itself to chain commands
    */
  def addFlag(name: String,
              short: Char,
              long: String,
              description: String = ""): Parser = {
    flags.append(FlagArgument(name, short, long, description))
    this
  }

  private val optionals: collection.mutable.ListBuffer[OptionalArgument[_]] =
    new ListBuffer[OptionalArgument[_]]

  /** add an optional argument
    *
    * @param name the name of the optional argument
    * @param short the short name for the optional argument
    * @param long the long name for the optional argument
    * @param process the function to process the cmd line string to the argument type
    * @param default the default value if no argument is provided by the user
    * @param description the description for the optional argument
    * @tparam T the type of the final value of the argument
    * @return the parser itself to chain commands
    */
  def addOptional[T](
      name: String,
      short: Char,
      long: String,
      process: String => T,
      default: Option[T] = None,
      description: String = "")(implicit tag: ClassTag[T]): Parser = {
    optionals.append(
      OptionalArgument[T](name, short, long, process, default, description)
    )
    this
  }

  private val subparsers: collection.mutable.ListBuffer[Parser] =
    new ListBuffer[Parser]

  def addSubparser(parser: Parser): Parser = {
    subparsers.append(parser.setParent(this))
    this
  }

  //todo: this should become private or package private
  override def parse(args: Iterable[String])(
      implicit result: ParsingResult): Iterable[String] = {
    if (args.nonEmpty) {
      if (args.head == name) {
        parseArgs(args.tail)(result)
        List()
      } else {
        args
      }
    } else {
      args
    }
  }

  def parseArgs(
      args: Iterable[String])(implicit parsingResult: ParsingResult =
                                new ParsingResult()): ParsingResult = {
    try {
      var current = args
      //1st process the default
      defaults.foreach(arg => arg.parse(current))
      //2nd process the positional
      positionals.foreach { positional =>
        current = positional.parse(args)
      }
      if (args.size - positionals.length != current.size) {
        throw new ParsingException(s"there weren't enough positionals provided",
                                   help())
      }
      var count = current.size
      //3rd process the optional and flags
      do {
        count = current.size
        optionals.foreach { optional =>
          current = optional.parse(current)
        }
        flags.foreach { flag =>
          current = flag.parse(current)
        }
      } while (current.size < count && current.nonEmpty)
      //5th process the subparsers
      if (subparsers.nonEmpty && current.isEmpty) {
        throw new ParsingException(
          s"a sub action has to be chosen '${subparsers.map(_.name).mkString("{", ",", "}")}'",
          help())
      }
      subparsers.foreach { subparser =>
        current = subparser.parse(current)
      }
      if (current.nonEmpty) {
        throw new ParsingException(
          s"we have remaining cmd arguments '${current.mkString(" ")}'",
          help())
      } else {
        parsingResult
      }
    } catch {
      case x: ParsingException =>
        throw new ParsingException(x.getMessage, x.getHelp)
      case HelpException =>
        throw new ParsingException("", help())
    }
  }

  protected def getCommandString: String = {
    val sb: StringBuilder = new StringBuilder()
    if (parent.isEmpty) {
      sb.append(s"./$name")
    } else {
      sb.append(name)
    }
    sb.append(positionals.map(name => s"[${name.name}]").mkString(" ", " ", ""))
    if (positionals.nonEmpty) sb.append(" ")
    sb.append(flags.map(_.short).mkString("", ",", ""))
    if (flags.nonEmpty && optionals.nonEmpty) sb.append(",")
    sb.append(optionals.map(_.shortFlag).mkString("", ",", ""))
    if (subparsers.nonEmpty)
      sb.append(subparsers.map(_.name).mkString(" {", ",", "}"))
    sb.toString().trim()
  }

  def createCommandString(current: Option[String]): String = {
    current match {
      case Some(value) =>
        parent match {
          case Some(pparser) =>
            pparser.createCommandString(Some(s"$name $value"))
          case None => s"./$name $value".trim()
        }
      case None =>
        parent match {
          case Some(pparent) =>
            pparent.createCommandString(Some(getCommandString))
          case None => getCommandString
        }
    }
  }

  override def help(): String = {
    val msg = new StringBuilder
    msg ++= s"usage: ${createCommandString(None)}".trim()
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
