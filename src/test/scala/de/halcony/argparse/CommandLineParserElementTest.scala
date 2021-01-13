package de.halcony.argparse

import org.scalatest.{Matchers, WordSpec}

class CommandLineParserElementTest extends WordSpec with Matchers {

  "Positional" should {
    "be able to parse a single element" in {
      implicit val result: ParsingResult = new ParsingResult()
      val pos = Positional("test","this is a description")
      pos.parse(List("test")).length shouldBe 0
      result.get[String]("test") shouldBe "test"
    }
    "be able to parse a single element out of multiple" in {
      implicit val result: ParsingResult = new ParsingResult()
      val pos = Positional("test","this is a description")
      pos.parse(List("test", "test")).length shouldBe 1
      result.get[String]("test") shouldBe "test"
    }
    "generate the proper help message" in {
      val pos = Positional("test","this is a description")
      pos.help() shouldBe "[test] this is a description"
    }
  }

  "Optional" should {
    "be able to parse a single element short" in {
      implicit val result: ParsingResult = new ParsingResult()
      val opt = Optional("test","t",default = Some("other"),description = "description")
      opt.parse(List("-t", "test")).length shouldBe 0
      result.get[Option[String]]("test") shouldBe Some("test")
    }
    "be able to parse a single element short out of multiple" in {
      implicit val result: ParsingResult = new ParsingResult()
      val opt = Optional("test","t","long", Some("default"),"description")
      opt.parse(List("-t", "test", "-n", "next")).length shouldBe 2
      result.get[Option[String]]("test") shouldBe Some("test")
    }
    "be able to parse a single element long" in {
      implicit val result: ParsingResult = new ParsingResult()
      val opt = Optional("test","t","long",Some("default"),"description")
      opt.parse(List("--long", "test")).length shouldBe 0
      result.get[Option[String]]("test") shouldBe Some("test")
    }
    "be able to parse a single element long out of multiple" in {
      implicit val result: ParsingResult = new ParsingResult()
      val opt = Optional("test","t","long",None,"description")
      opt.parse(List("--long", "test", "-n", "next")).length shouldBe 2
      result.get[Option[String]]("test") shouldBe Some("test")
    }
    "be able to generate help message with just short" in {
      //implicit val result: ParsingResult = new ParsingResult()
      val opt = Optional("test","t",default = Some("other"),description = "description")
      opt.help() shouldBe "-t <value> description (def:other)"
    }
    "be able to generate help message with short and long" in {
      //implicit val result: ParsingResult = new ParsingResult()
      val opt = Optional("test","t","test", default = Some("other"),description = "description")
      opt.help() shouldBe "-t/--test <value> description (def:other)"
    }
  }

  "default" should {
    "be able to store default integer value" in {
      val default = Default("test",42,"default")
      default.value.asInstanceOf[Integer] shouldBe 42
    }
    "be abel to store a function" in {
      val default = Default("func",(x:Int) => x * x, "test function")
      default.value.apply(4) shouldBe 16
    }
  }

  "Flag" should {
    "be able to parse a single element" in {
      implicit val result: ParsingResult = new ParsingResult()
      val flag = Flag("test","t",description = "description")
      flag.parse(List("-t", "test")).length shouldBe 1
      result.get[Boolean]("test") shouldBe true
    }
    "be able to generate a help message" in {
      val flag = Flag("test","t","test","description")
      flag.help() shouldBe "-t/--test description"
    }
  }
}
