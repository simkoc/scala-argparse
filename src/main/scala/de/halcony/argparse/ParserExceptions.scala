package de.halcony.argparse

import scala.annotation.nowarn

class ParsingException(message: String, help: String) extends Exception {

  override def getMessage: String = message

  def getHelp: String = help

  @nowarn
  def getContextHelp: String = {
    s"""$message
       |
       |$help
       |""".stripMargin
  }

}

class UnknownValue(message: String) extends Exception {

  override def getMessage: String = message

}
