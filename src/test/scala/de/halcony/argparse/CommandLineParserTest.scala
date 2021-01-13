package de.halcony.argparse

import org.scalatest.{Matchers, WordSpec}

class CommandLineParserTest  extends WordSpec with Matchers {

  "Parser" should {
    "be able to parse single of each (no sub)" in {
      val parser = Parser("test", "test")
        .addPositional("pos1")
        .addOptional("opt1", "o")
        .addFlag("flag1", "f")
      val result = parser.parseArgv(List("value", "-o", "optional", "-f"))
      result.get[String]("pos1") shouldBe "value"
      result.get[Option[String]]("opt1") shouldBe Some("optional")
      result.get[Boolean]("flag1") shouldBe true
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
      val result = parser.parseArgv(List("value", "next", "otherValue"))
      result.get[String]("pos1") shouldBe "value"
      result.get[String]("pos2") shouldBe "otherValue"
    }
    "be able to work with default values" in {
      val parser = Parser("test", "test")
        .addDefault[Int => Int]("func", (x: Int) => x * x, "default function")
      val result = parser.parseArgv(List(""))
      result.get[Int => Int]("func").apply(4) shouldBe 16
    }
    "be able to work with default values in subparser" in {
      val parser = Parser("test", "test")
        .addSubparser(Parser("square", "square")
          .addDefault[Int => Int]("func", (x: Int) => x * x, "default function")
        )
        .addSubparser(Parser("add", "add")
          .addDefault[Int => Int]("func", (x: Int) => x + x, "default function")
        )
      val result = parser.parseArgv(List("square"))
      result.get[Int => Int]("func").apply(4) shouldBe 16
    }
    "be able to work with default values in subparser for either" in {
      val parser = Parser("test", "test")
        .addSubparser(Parser("square", "square")
          .addDefault[Int => Int]("func", (x: Int) => x * x, "default function")
        )
        .addSubparser(Parser("add", "add")
          .addDefault[Int => Int]("func", (x: Int) => x + x, "default function")
        )
      val result = parser.parseArgv(List("add"))
      result.get[Int => Int]("func").apply(4) shouldBe 8
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
      val result = parser.parseArgv(List("-o", "optional", "relevant", "positional", "-l", "number", "-f"))
      result.toMap shouldBe Map(
        "optional" -> Some("optional"),
        "first" -> "positional",
        "option" -> Some("number"),
        "flag" -> true
      )
    }
  }

}
