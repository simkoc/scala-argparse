package de.halcony.argparse.parsing

import de.halcony.argparse.ParsingResult
import org.scalatest.{Matchers, WordSpec}

class CommandLineParserElementTest extends WordSpec with Matchers {

  "Positional" should {
    "be able to parse a single element" in {
      implicit val result: ParsingResult = new ParsingResult()
      val pos = PositionalArgument("test","this is a description")
      pos.parse(List("test")).toList.length shouldBe 0
      result.get[String]("test") shouldBe "test"
    }
    "be able to parse a single element out of multiple" in {
      implicit val result: ParsingResult = new ParsingResult()
      val pos = PositionalArgument("test","this is a description")
      pos.parse(List("test", "test")).toList.length shouldBe 1
      result.get[String]("test") shouldBe "test"
    }
    "generate the proper help message" in {
      val pos = PositionalArgument("test","this is a description")
      pos.help() shouldBe "[test] this is a description"
    }
  }

  "Optional" should {
    "be able to parse a single element short" in {
      implicit val result: ParsingResult = new ParsingResult()
      val opt = OptionalArgument("test",'t',"test",identity[String],default = Some("other"),description = "description")
      opt.parse(List("-t", "test")).toList.length shouldBe 0
      result.get[String]("test") shouldBe "test"
    }
    "be able to parse a single element short out of multiple" in {
      implicit val result: ParsingResult = new ParsingResult()
      val opt = OptionalArgument("test",'t',"long",identity[String],Some("default"),"description")
      opt.parse(List("-t", "test", "-n", "next")).toList.length shouldBe 2
      result.get[String]("test") shouldBe "test"
    }
    "be able to parse a single element long" in {
      implicit val result: ParsingResult = new ParsingResult()
      val opt = OptionalArgument("test",'t',"long",identity[String],Some("default"),"description")
      opt.parse(List("--long", "test")).toList.length shouldBe 0
      result.get[String]("test") shouldBe "test"
    }
    "be able to parse a single element long out of multiple" in {
      implicit val result: ParsingResult = new ParsingResult()
      val opt = OptionalArgument("test", 't',"long", identity[String], None, "description")
      opt.parse(List("--long", "test", "-n", "next")).toList.length shouldBe 2
      result.get[String]("test") shouldBe "test"
    }
    "be able to generate help message with short and long" in {
      //implicit val result: ParsingResult = new ParsingResult()
      val opt = OptionalArgument("test",'t',"test", identity[String],  default = Some("other"),description = "description")
      opt.help() shouldBe "-t/--test <value> description (default:other)"
    }
    "be able to generate help message with short and long no default" in {
      //implicit val result: ParsingResult = new ParsingResult()
      val opt = OptionalArgument("test",'t',"test", identity[String],  default = None,description = "description")
      opt.help() shouldBe "-t/--test <value> description"
    }
  }

  "default" should {
    "be able to store default integer value" in {
      val default = DefaultArgument("test",42)
      default.value.asInstanceOf[Integer] shouldBe 42
    }
    "be abel to store a function" in {
      val default = DefaultArgument("func",(x:Int) => x * x)
      default.value.apply(4) shouldBe 16
    }
  }

  "Flag" should {
    "be able to parse a single element" in {
      implicit val result: ParsingResult = new ParsingResult()
      val flag = FlagArgument("test",'t',"test", description = "description")
      flag.parse(List("-t", "test")).toList.length shouldBe 1
      result.get[Boolean]("test") shouldBe true
    }
    "be able to generate a help message" in {
      val flag = FlagArgument("test",'t',"test","description")
      flag.help() shouldBe "-t/--test description"
    }
  }
}
