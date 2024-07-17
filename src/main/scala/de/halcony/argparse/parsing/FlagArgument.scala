package de.halcony.argparse.parsing

import de.halcony.argparse.ParsingResult

class FlagArgument protected (override val name: String,
                              shortName: Char,
                              longName: String = "",
                              override val description: String = "")
    extends ParsedArgument[Boolean](
      name,
      description,
      _ => throw new RuntimeException("no parsing of flag arguments")) {

  val short: String = s"-$shortName"
  val long: String = s"--$longName"

  override def parse(args: Iterable[String])(
      implicit result: ParsingResult): Iterable[String] = {
    if (result.getOptional[Boolean](name).isEmpty) {
      result.addResult(name, new ResultValue[Boolean](value = false))
    }
    if (args.nonEmpty) {
      if (short == args.head || long == args.head) {
        result.addResult(name, new ResultValue[Boolean](value = true))
        args.tail
      } else {
        args
      }
    } else {
      args
    }
  }

  override def help(): String = s"$short/$long $description"

}

object FlagArgument {

  def apply(name: String,
            short: Char,
            long: String,
            description: String = ""): FlagArgument = {
    new FlagArgument(name, short, long, description)
  }

}
