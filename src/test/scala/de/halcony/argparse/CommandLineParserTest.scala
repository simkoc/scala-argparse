package de.halcony.argparse

import org.scalatest.{Matchers, WordSpec}

class CommandLineParserTest  extends WordSpec with Matchers {

  "Parser" should {
    "be able to parse single of each (no sub)" in {
      val parser = Parser("test", "test")
        .addPositional("pos1")
        .addOptional("opt1", "o")
        .addFlag("flag1", "f")
      val result = parser.parse(Array("value", "-o", "optional", "-f"))
      result.getValue[String]("pos1") shouldBe "value"
      result.get[SomeOptionalValue[String]]("opt1").value shouldBe Some("optional")
      result.getValue[Boolean]("flag1") shouldBe true
    }
    "be able to handle single subparser" in {
      val parser = Parser("test", "test")
        .addPositional("pos1")
        .addSubparser {
          Parser("next", "next").addPositional("pos2")
        }
        .addSubparser {
          Parser("other", "other").addPositional("pos3")
        }
      val result = parser.parse(Array("value", "next", "otherValue"))
      result.getValue[String]("pos1") shouldBe "value"
      result.getValue[String]("pos2") shouldBe "otherValue"
    }
    "be able to work with default values" in {
      val parser = Parser("test", "test")
        .addDefault[Int => Int]("func", (x: Int) => x * x, "default function")
      val result = parser.parse(Array(""))
      result.getValue[Int => Int]("func").apply(4) shouldBe 16
    }
    "be able to work with default values in subparser" in {
      val parser = Parser("test", "test")
        .addSubparser(Parser("square", "square")
          .addDefault[Int => Int]("func", (x: Int) => x * x, "default function")
        )
        .addSubparser(Parser("add", "add")
          .addDefault[Int => Int]("func", (x: Int) => x + x, "default function")
        )
      val result = parser.parse(Array("square"))
      result.getValue[Int => Int]("func").apply(4) shouldBe 16
    }
    "be able to work with default values in subparser for either" in {
      val parser = Parser("test", "test")
        .addSubparser(Parser("square", "square")
          .addDefault[Int => Int]("func", (x: Int) => x * x, "default function")
        )
        .addSubparser(Parser("add", "add")
          .addDefault[Int => Int]("func", (x: Int) => x + x, "default function")
        )
      val result = parser.parse(Array("add"))
      result.getValue[Int => Int]("func").apply(4) shouldBe 8
      result.get[DefaultValue[Int => Int]]("func").value.apply(4) shouldBe 8
    }
    "be able to parse mixture of flag in subparser" in {
      val parser = Parser("test", "test")
        .addOptional("optional", "o", "optional", Some("test"), "documentation")
        .addSubparser {
          Parser("ignore", "ignore")
            .addPositional("ignorepos", "ignore me")
        }
        .addSubparser {
          Parser("relevant", "relevant")
            .addPositional("first", "first")
            .addOptional("option", "l", "option")
            .addFlag("flag", "f", "flag")
        }
      val result = parser.parse(Array("-o", "optional", "relevant", "positional", "-l", "number", "-f"))
      result.toMap shouldBe Map(
        "optional" -> Some("optional"),
        "first" -> "positional",
        "option" -> Some("number"),
        "help" -> false,
        "flag" -> true
      )
    }
    "be able to parse optional parameter out of order" in {
      val parser = Parser("test","test")
        .addOptional("option1","o","optional1",Some("test"))
        .addOptional("option2","c","option2",Some("other"))
      val result = parser.parse(Array("-c","ttt","-o","ssss"))
      result.toMap shouldBe Map(
        "option1" -> Some("ssss"),
        "help" -> false,
        "option2" -> Some("ttt")
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
            .addPositional("bp", "test")
            .addOptional("out",
              "o",
              "output",
              Some("out"))
            .addOptional("lin",
              "l",
              "linking",
              Some("strict"))
            .addOptional("end",
              "e",
              "endings",
              Some("endings"))
            .addOptional("con",
              "c",
              "con",
              None)
            .addDefault[String]("ident","unique")
        )
      val result = mainParser.parse(Array("create", "positional", "-l","lparam","-o","oparam","-c","cparam"))
      result.toMap shouldBe Map(
        "ident" -> "unique",
        "lin" -> Some("lparam"),
        "out" -> Some("oparam"),
        "con" -> Some("cparam"),
        "help" -> false,
        "end" -> Some("endings"),
        "bp" -> "positional"
      )
    }
  }

}
