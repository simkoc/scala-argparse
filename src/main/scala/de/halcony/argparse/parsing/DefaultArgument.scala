package de.halcony.argparse.parsing

import de.halcony.argparse.ParsingResult

case class DefaultArgument[T] protected (override val name: String,
                                         value: T,
                                         override val description: String = "")
    extends BaseArgument(name, description) {

  override def parse(cmd: Iterable[String])(
      implicit pargv: ParsingResult): Iterable[String] = {
    pargv.addResult(name, ResultValue[T](value))
    cmd
  }

}

object DefaultArgument {

  def apply[T](name: String, value: T): DefaultArgument[T] = {
    new DefaultArgument[T](name, value)
  }

}
