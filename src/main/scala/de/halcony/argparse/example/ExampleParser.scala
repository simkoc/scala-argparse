package de.halcony.argparse.example

import de.halcony.argparse.{Parser, ParsingException, ParsingResult}

object ExampleParser {

  val parser : Parser = Parser("Example Parser", "example parser to show and test functionality")
    .addFlag("stop","s","stop","halts everything and terminates the program immediately")
    .addOptional("message","m","message",None,"the message displayed if the program is stopped via flag")
    .addSubparser {
      Parser("first", "the first subparser branch")
      .addDefault[ParsingResult => Unit]("func", first)
    }
    .addSubparser {
      Parser("second", "the second subparser branch")
        .addDefault[ParsingResult => Unit]("func", second)
    }

  def first(pargs : ParsingResult) : Unit = {
    println("running first")
    println(pargs.toMap.foreach(pair => println(s"${pair._1} -> ${pair._2}")))
  }

  def second(pargs : ParsingResult) : Unit = {
    println("running second")
    println(pargs.toMap.foreach(pair => println(s"${pair._1} -> ${pair._2}")))
  }

  def main(argv : Array[String]) : Unit = {
    try {
      println(argv.toList)
      val pargs: ParsingResult = parser.parse(argv)
      if (pargs.get[Boolean]("stop")) {
        println("stop message:" + pargs.get[Option[String]]("message").getOrElse("No stop message provided"))
        sys.exit()
      }
      pargs.get[ParsingResult => Unit]("func")(pargs)
    } catch {
      case _ : ParsingException =>
    }
  }
}
