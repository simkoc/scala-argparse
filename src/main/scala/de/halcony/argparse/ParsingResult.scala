package de.halcony.argparse

sealed trait Result

sealed trait ParsingResultValue[T] extends Result {

  val value : T

}

sealed trait OptionalParsingResultValue[T] extends ParsingResultValue[T] {

  val provided : Boolean

}

case class DefaultValue[T](override val value: T) extends ParsingResultValue[T]

case class PositionalValue[T](override val value : T) extends ParsingResultValue[T]

case class OptionalValue[T](override val value : T, override val provided : Boolean) extends OptionalParsingResultValue[T]

case class FlagValue(override val value : Boolean, override val provided : Boolean) extends OptionalParsingResultValue[Boolean]

case class UnknownArgument(name : String, atype : Option[String]) extends Throwable {
  override def toString: String = atype match {
    case Some(value) => s"expected $value argument $name but none found"
    case None => s"expected argument $name but none found"
  }
}

object UnknownArgument {

  def apply(name : String) : UnknownArgument = new UnknownArgument(name,None)
  def apply(name : String, atype : String) : UnknownArgument = new UnknownArgument(name,Some(atype))

}

class ParsingResult() {

  private val results: collection.mutable.Map[String, Result] =
    collection.mutable.Map()


  private[argparse] def addResult[T <: Result](name: String, value: T): Unit = {
    results.addOne((name, value))
  }

  def get[T <: Result](name: String): T = {
    results.get(name) match {
      case Some(x : T) => x
      case None => throw UnknownArgument(name)
    }
  }

  def getValue[T](name : String) : T = {
    results.get(name) match {
      case Some(x : ParsingResultValue[T]) => x.value
      case None => throw UnknownArgument(name)
    }
  }

  def getOrElse[T <: Result](name: String, otherwise: => T): T = {
    results.getOrElse(name, otherwise) match {
      case x : T => x
    }
  }

  def getValueOrElse[T](name : String, otherwise: => T) : T = {
    results.getOrElse(name, otherwise) match {
      case x : OptionalParsingResultValue[Option[T]] => x.value.getOrElse(otherwise)
      case x : ParsingResultValue[T] => x.value
    }
  }

  override def toString: String = {
    results.iterator
      .map { pair =>
        s"${pair._1} -> ${pair._2.toString}"
      }
      .mkString("\n")
  }

  def toMap: Map[String, AnyVal] = {
    results.map { pair =>
      pair._1 -> pair._2.asInstanceOf[ParsingResultValue[AnyVal]].value
    }.toMap
  }

}
