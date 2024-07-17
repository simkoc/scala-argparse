package de.halcony.argparse.parsing

import de.halcony.argparse.ParsingResult

object HelpException extends Throwable

object HelpFlag
    extends FlagArgument("help", 'h', "help", "show this help dialog") {

  override def parse(args: Iterable[String])(
      implicit result: ParsingResult): Iterable[String] = {
    if (args.nonEmpty && (args.head == this.short || args.head == this.long)) {
      throw HelpException
    } else {
      result.addResult("help", new ResultValue[Boolean](false))
      args
    }
  }

}
