package de.halcony.argparse.parsing

import de.halcony.argparse.ParsingResult

import scala.annotation.nowarn
import scala.reflect.ClassTag

@nowarn
class PositionalArgument[T] protected (
    name: String,
    description: String,
    process: String => T)(implicit tag: ClassTag[T])
    extends ParsedArgument[T](name, description, process) {

  override def parse(args: Iterable[String])(
      implicit result: ParsingResult): Iterable[String] = {
    if (args.nonEmpty) {
      result.addResult(name, new ResultValue[T](process(args.head)))
      args.tail
    } else {
      args
    }
  }

  override def help(): String = {
    s"[$name] $description"
  }
}

object PositionalArgument {

  def apply(name: String, description: String): PositionalArgument[String] = {
    new PositionalArgument[String](name, description, value => value)
  }

  def apply[T](name: String, description: String, process: String => T)(
      implicit tag: ClassTag[T]): PositionalArgument[T] = {
    new PositionalArgument[T](name, description, process)
  }

}
