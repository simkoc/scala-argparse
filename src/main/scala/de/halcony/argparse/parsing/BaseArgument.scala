package de.halcony.argparse.parsing

import de.halcony.argparse.ParsingResult

abstract class BaseArgument(val name: String, val description: String) {

  def parse(cmd: Iterable[String])(
      implicit pargv: ParsingResult): Iterable[String]

}
