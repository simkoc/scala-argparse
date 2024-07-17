package de.halcony.argparse

import de.halcony.argparse.parsing.{Result, ResultValue}

import scala.annotation.nowarn
import scala.reflect.ClassTag

class ParsingResult {

  private val results: collection.mutable.Map[String, Result] =
    collection.mutable.Map()

  private[argparse] def addResult[T <: Result](name: String, value: T): Unit = {
    results.addOne((name, value))
  }

  private[argparse] def toList: List[(String, Result)] =
    results.toList.sortBy(_._1)

  def get[T <: Any](name: String)(implicit tag: ClassTag[T]): T = {
    results.get(name) match {
      case Some(value) =>
        value.asInstanceOf[ResultValue[_]].value match {
          case t: T =>
            t
          case x =>
            throw new UnknownValue(
              s"there is a value of $name but it is of type ${x.getClass}")
        }
      case None => throw new UnknownValue(s"there is no value name $name")
    }
  }

  @nowarn
  def getFLag(name: String): Boolean = {
    get[Boolean](name)
  }

  @nowarn
  def getOptional[T <: Any](name: String)(
      implicit tag: ClassTag[T]): Option[T] = {
    try {
      Some(get[T](name))
    } catch {
      case _: UnknownValue =>
        None
    }
  }

  @nowarn
  def getOptionalOrElse[T <: Any](name: String, orElse: => T)(
      implicit tag: ClassTag[T]): T = {
    get[Option[T]](name) match {
      case Some(value) => value
      case None        => orElse
    }
  }

}
