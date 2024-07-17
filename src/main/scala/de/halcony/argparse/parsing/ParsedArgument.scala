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
