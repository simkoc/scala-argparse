package de.halcony.argparse.parsing

import de.halcony.argparse.ParsingResult

import scala.reflect.ClassTag

class OptionalArgument[T] protected (
    name: String,
    short: Char,
    long: String,
    default: Option[T],
    description: String,
    process: String => T)(implicit tag: ClassTag[T])
    extends ParsedArgument[T](name, description, process) {

  val shortFlag = s"-$short"
  val longFlag = s"--$long"

  override def parse(args: Iterable[String])(
      implicit result: ParsingResult): Iterable[String] = {
    if (result.getOptional[T](name).isEmpty) {
      // and we have not encountered the flag before
      default match {
        case Some(default) =>
          // we add the default value if it exists
          result.addResult(name, new ResultValue[T](default))
        case None =>
      }
    }
    if (args.nonEmpty) {
      // if we encounter the proper flag
      if (args.head == shortFlag || args.head == longFlag) {
        // add the following string value after processing it
        result.addResult(name, new ResultValue[T](process(args.tail.head)))
        // reduce the command line arguments by two elements
        args.tail.tail
      } else {
        // if we do not encounter the flag
        // do not reduce the command line arguments
        args
      }
    } else {
      args
    }
  }

  override def help(): String = {
    s"$shortFlag/$longFlag <value> $description${default match {
      case Some(value) => s" (default:${value.toString})"
      case None        => ""
    }}"
  }
}

object OptionalArgument {

  def apply[T](name: String,
               short: Char,
               long: String,
               process: String => T,
               default: Option[T] = None,
               description: String = "")(
      implicit tag: ClassTag[T]): OptionalArgument[T] = {
    new OptionalArgument[T](name, short, long, default, description, process)
  }

}
