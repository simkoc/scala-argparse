package de.halcony.argparse

import org.scalatest.{WordSpec,Matchers}

class CommandLineParserTest extends WordSpec with Matchers {

  "Positional" should {
    "be able to parse a single element" in {
      val pos = Positional("test","this is a description")
      pos.parse(List("test")).length shouldBe 0
      pos.value shouldBe "test"
    }
    "be able to parse a single element out of multiple" in {
      val pos = Positional("test","this is a description")
      pos.parse(List("test", "test")).length shouldBe 1
      pos.value shouldBe "test"
    }
    "generate the proper help message" in {
      val pos = Positional("test","this is a description")
      pos.help() shouldBe "[test] this is a description"
    }
  }

  "Optional" should {
    "be able to parse a single element short" in {
      val opt = Optional("test","t",default = Some("other"),description = "description")
      opt.parse(List("-t", "test")).length shouldBe 0
      opt.value shouldBe Some("test")
    }
    "be able to parse a single element short out of multiple" in {
      val opt = Optional("test","t","long",None,"description")
      opt.parse(List("-t", "test", "-n", "next")).length shouldBe 2
      opt.value shouldBe Some("test")
    }
    "be able to parse a single element long" in {
      val opt = Optional("test","t","long",None,"description")
      opt.parse(List("--long", "test")).length shouldBe 0
      opt.value shouldBe Some("test")
    }
    "be able to parse a single element long out of multiple" in {
      val opt = Optional("test","t","long",None,"description")
      opt.parse(List("--long", "test", "-n", "next")).length shouldBe 2
      opt.value shouldBe Some("test")
    }
    "be able to generate help message with just short" in {
      val opt = Optional("test","t",default = Some("other"),description = "description")
      opt.help() shouldBe "-t <value> description (def:other)"
    }
    "be able to generate help message with short and long" in {
      val opt = Optional("test","t","test", default = Some("other"),description = "description")
      opt.help() shouldBe "-t/--test <value> description (def:other)"
    }
  }

  "Flag" should {
    "be able to parse a single element" in {
      val flag = Flag("test","t",description = "description")
      flag.parse(List("-t", "test")).length shouldBe 1
      flag.value shouldBe true
    }
    "be able to generate a help message" in {
      val flag = Flag("test","t","test","description")
      flag.help() shouldBe "-t/--test description"
    }
  }

  "Parser" should {
    "be able to parse single of each (no sub)" in {
      val parser = Parser("test","test")
        .addPositional("pos1")
        .addOptional("opt1","o")
        .addFlag("flag1","f")
      parser.parse(List("value","-o","optional","-f")).length shouldBe 0
      parser.get[Positional]("pos1").value shouldBe "value"
      parser.get[Optional]("opt1").value.get shouldBe "optional"
      parser.get[Flag]("flag1").value shouldBe true
    }
    "be able to handle single subparser" in {
      val parser = Parser("test","test")
        .addPositional("pos1")
        .addSubparser{
          Parser("next","next").addPositional("pos2")
        }
        .addSubparser{
          Parser("other","other").addPositional("pos3")
        }
      parser.parse(List("value","next","otherValue")).length shouldBe 0
      parser.get[Positional]("pos1").value shouldBe "value"
      parser.get[Parser]("next").get[Positional]("pos2").value shouldBe "otherValue"
    }
    "print a proper help message" in {
      val parser = Parser("test","test")
        .addPositional("pos1", "description")
        .addOptional("opt1","o", description = "stuff and stuff")
        .addFlag("flag1","f", description = "other stuff")
        .addSubparser{
          Parser("other","other").addPositional("pos3")
        }
      parser.help() shouldBe
        """usage: test [pos1] {o,f} other

test

[pos1] description
-o <value> stuff and stuff
-f other stuff
other other"""
    }
  }
}
