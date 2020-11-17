package de.halcony.argparse

class ParsingException(message: String) extends Exception {

  override def getMessage: String = message

}

private[argparse] class IntermediateParsingException(message: String)
    extends Exception {

  override def getMessage: String = message

}
