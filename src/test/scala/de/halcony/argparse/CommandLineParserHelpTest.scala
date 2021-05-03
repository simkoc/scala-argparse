package de.halcony.argparse

import org.scalatest.{Matchers, WordSpec}

class CommandLineParserHelpTest extends WordSpec with Matchers {

  "the help message" should {

    "be correctly formed for optionals" in {
      val parser = Parser("test","test")
        .addOptional("optional1","o","opt1", Some("first"), "this is the first optional")
        .addOptional("optional2","c","opt2", Some("second"), "this is the second optional")
      parser.help() shouldBe
        """usage: test {-o,-c,-h}
          |
          |test
          |
          |          -o/--opt1 <value> this is the first optional (def:first)
          |
          |          -c/--opt2 <value> this is the second optional (def:second)
          |
          |          -h/--help prints this help message
          |
          |
          |""".stripMargin
    }

    "be correctly formed for positionals" in {
      val parser = Parser("test","test")
        .addPositional("first","this is a positional")
        .addPositional("second", "this is another positional")
      parser.help() shouldBe
        """usage: test [first] [second] {-h}
          |
          |test
          |
          |          [first] this is a positional
          |
          |          [second] this is another positional
          |
          |          -h/--help prints this help message
          |
          |
          |""".stripMargin
    }

    "be correctly formed for flags" in {
      val parser = Parser("test","test")
        .addFlag("first","f","flag1","the first flag")
        .addFlag("second","s","flag2", "the second flag")
      parser.help() shouldBe
        """usage: test {-h,-f,-s}
          |
          |test
          |
          |          -h/--help prints this help message
          |
          |          -f/--flag1 the first flag
          |
          |          -s/--flag2 the second flag
          |
          |
          |""".stripMargin

    }

    "be correctly formated in mixture" in {
        val parser = Parser("test","test")
          .addOptional("optional1","o","opt1", Some("first"), "this is the first optional")
          .addOptional("optional2","c","opt2", Some("second"), "this is the second optional")
          .addPositional("first","this is a positional")
          .addPositional("second", "this is another positional")
          .addFlag("first","f","flag1","the first flag")
          .addFlag("second","s","flag2", "the second flag")
      parser.help() shouldBe
        """usage: test [first] [second] {-o,-c,-h,-f,-s}
          |
          |test
          |
          |          [first] this is a positional
          |
          |          [second] this is another positional
          |
          |          -o/--opt1 <value> this is the first optional (def:first)
          |
          |          -c/--opt2 <value> this is the second optional (def:second)
          |
          |          -h/--help prints this help message
          |
          |          -f/--flag1 the first flag
          |
          |          -s/--flag2 the second flag
          |
          |
          |""".stripMargin
    }

    "be correctly formed with subparsers available" in {
      val parser = Parser("test","test")
        .addSubparser{
          Parser("sub","sub")
        }
        .addSubparser{
          Parser("sub2","sub2")
        }
      parser.help() shouldBe
        """usage: test {-h} sub sub2
          |
          |test
          |
          |          -h/--help prints this help message
          |
          |          sub sub
          |
          |          sub2 sub2
          |
          |
          |""".stripMargin
    }

  }

}
