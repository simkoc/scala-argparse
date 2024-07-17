package de.halcony.argparse.example

import de.halcony.argparse.{Parser, ParsingException, ParsingResult}

import scala.annotation.nowarn

object ExampleParser {

  val parser: Parser =
    Parser("Example Parser", "example parser to show and test functionality")
      .addPositional("stuff", identity[String], "positional")
      .addPositional("stuff2", identity[String], "positional")
      .addFlag("stop",
               's',
               "stop",
               "halts everything and terminates the program immediately")
      .addOptional("message",
                   'm',
                   "message",
                   identity[String],
                   description =
                     "the message displayed if the program is stopped via flag")
      .addSubparser {
        Parser("first", "the first subparser branch")
          .addPositional("positional",
                         identity[String],
                         "a positional that has to be provided")
          .addFlag("flag", 'f', "flag", "a flag that can be set")
          .addOptional("optional",
                       'o',
                       "optional",
                       identity[String],
                       Option("default"),
                       "an optional parameter")
          .addDefault[ParsingResult => Unit]("func", first)
      }
      .addSubparser {
        Parser("second", "the second subparser branch")
          .addDefault[ParsingResult => Unit]("func", second)
      }

  def first(pargs: ParsingResult): Unit = {
    println("running first")
    println(pargs.get[String]("positional"))
    println(pargs.getFLag("flag"))
    println(pargs.getOptional[String]("optional"))
    println(pargs.get[String]("optional"))
  }

  def second(@nowarn pargs: ParsingResult): Unit = {
    println("running second")
  }

  def main(argv: Array[String]): Unit = {
    try {
      println(argv.toList)
      val pargs: ParsingResult = parser.parseArgs(argv)
      if (pargs.getFLag("stop")) {
        println(
          "stop message:" + pargs
            .getOptionalOrElse[String]("message", "No stop message provided"))
        scala.sys.exit(0)
      }
      val func = pargs.get[ParsingResult => Unit]("func")
      func(pargs)
    } catch {
      case x: ParsingException =>
        println(x.getContextHelp)
    }
  }
}
