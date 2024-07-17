package de.halcony.argparse.parsing

trait Result

case class ResultValue[T](value: T) extends Result
