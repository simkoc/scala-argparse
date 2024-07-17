package de.halcony.argparse

import de.halcony.argparse.parsing.{Result, ResultValue}
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.HashMap

class CommandLineParserTest  extends WordSpec with Matchers {

  "Parser" should {

    "be able to parse single of each (no sub)" in {
      val parser = Parser("test", "test")
        .addPositional("pos1", identity[String])
        .addOptional("opt1", 'o', "opt1", identity[String])
        .addFlag("flag1", 'f',"flag1")
      val result = parser.parseArgs(Array("value", "-o", "optional", "-f"))
      result.get[String]("pos1") shouldBe "value"
      result.get[String]("opt1") shouldBe "optional"
      result.get[Boolean]("flag1") shouldBe true
    }

    "be able to handle single subparser" in {
      val parser = Parser("test", "test")
        .addPositional("pos1", identity[String])
        .addSubparser {
          Parser("next", "next").addPositional("pos2", identity[String])
        }
        .addSubparser {
          Parser("other", "other").addPositional("pos3", identity[String])
        }
      val result = parser.parseArgs(Array("value", "next", "otherValue"))
      result.get[String]("pos1") shouldBe "value"
      result.get[String]("pos2") shouldBe "otherValue"
    }
    "be able to work with default values" in {
      val parser = Parser("test", "test")
        .addDefault[Int => Int]("func", (x: Int) => x * x)
      val result = parser.parseArgs(Array[String]())
      result.get[Int => Int]("func").apply(4) shouldBe 16
    }
    "be able to work with default values in subparser" in {
      val parser = Parser("test", "test")
        .addSubparser(Parser("square", "square")
          .addDefault[Int => Int]("func", (x: Int) => x * x)
        )
        .addSubparser(Parser("add", "add")
          .addDefault[Int => Int]("func", (x: Int) => x + x)
        )
      val result = parser.parseArgs(Array("square"))
      result.get[Int => Int]("func").apply(4) shouldBe 16
    }
    "be able to work with default values in subparser for either" in {
      val parser = Parser("test", "test")
        .addSubparser(Parser("square", "square")
          .addDefault[Int => Int]("func", (x: Int) => x * x)
        )
        .addSubparser(Parser("add", "add")
          .addDefault[Int => Int]("func", (x: Int) => x + x)
        )
      val result = parser.parseArgs(Array("add"))
      result.get[Int => Int]("func").apply(4) shouldBe 8
      result.get[Int => Int]("func").apply(4) shouldBe 8
    }
    "be able to parse mixture of flag in subparser" in {
      val parser = Parser("test", "test")
        .addOptional("optional", 'o', "optional", identity[String], Some("test"), "documentation")
        .addSubparser {
          Parser("ignore", "ignore")
            .addPositional("ignorepos", identity[String], "ignore me")
        }
        .addSubparser {
          Parser("relevant", "relevant")
            .addPositional("first", identity[String], "first")
            .addOptional("option", 'l', "option", identity[String])
            .addFlag("flag", 'f', "flag")
        }
      val result = parser.parseArgs(Array("-o", "optional", "relevant", "positional", "-l", "number", "-f"))

      result.toList shouldBe List(
        "first" -> ResultValue("positional"),
        "flag" -> ResultValue(true),
        "option" -> ResultValue("number"),
        "optional" -> ResultValue("optional"),
      )
    }
    "be able to parse optional parameter out of order" in {
      val parser = Parser("test","test")
        .addOptional("option1",'o', "option1", identity[String], Some("test"))
        .addOptional("option2",'c', "option2", identity[String] ,Some("other"))
      val result = parser.parseArgs(Array("-c","ttt","-o","ssss"))
      result.toList shouldBe List(
        "option1" -> ResultValue("ssss"),
        "option2" -> ResultValue("ttt")
      )
    }
    "be able to ignore order of optional parameters in more complex context" in {
      val mainParser: Parser = de.halcony.argparse
        .Parser(
          "test",
          "test"
        )
        .addSubparser(
          de.halcony.argparse
            .Parser("create", "test")
            .addPositional("bp", identity[String], "test")
            .addOptional("out",
              'o',
              "output",
              identity[String],
              Some("out"))
            .addOptional("lin",
              'l',
              "linking",
              identity[String],
              Some("strict"))
            .addOptional("end",
              'e',
              "endings",
              identity[String],
              Some("endings"))
            .addOptional("con",
              'c',
              "con",
              identity[String],
              None)
            .addDefault[String]("ident","unique")
        )
      val result = mainParser.parseArgs(Array("create", "positional", "-l","lparam","-o","oparam","-c","cparam"))
      result.toList shouldBe List(
        "bp" -> ResultValue("positional"),
        "con" -> ResultValue("cparam"),
        "end" -> ResultValue("endings"),
        "ident" -> ResultValue("unique"),
        "lin" -> ResultValue("lparam"),
        "out" -> ResultValue("oparam"),
      )
    }
  }

}
