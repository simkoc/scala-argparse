# scala argparse

A python argparse inspired library to parse command line arguments


## Install using sbt

You need to add the dependency
```
    "de.halcony"                 %% "scala-argparse"                % "(version)"
```

as well as the resolver

```
resolvers ++= Seq(
    "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/public",
)
```

### Usage

```
import de.halcony.argparse._

def main(argv : Array[String]) : Unit = {
    val parser = Parser("example parser",
                        "this is the description for the example parser")
        .addPosition("positional-name","positional description")
        .addOptional("optional-name","o","optional-long-parameter-name",Some("default-value"),"the description")
        .addFlag("flag-name","f","flag-long-name","the description")
        .addDefault[<hereIsTheType]("default-value-name", defaultValueValue)
        .addSubparser(Parser("subparser-name","subparser description")
            ...
            /* and so on and so forth */
        
    val pargs = parser.parseArgv(argv.toList)
    pargs.getValue[Boolean]("flag-name")
    pargs.getValue[String]("optional-name")
    pargs.getValue[String]("positional-name")
    pargs.getValue[<hereIsTheType>]("default-value-name") 
```

```
./program positional-value -o optional-value -f subparser-name .... 
```

There is theoretically a help flag triggered by `-h/--help` however this tends to conflict with the help flag of java itself.

The hierarchy of parsers is flattened concerning for the results using the path described by the arguments. 
The last (i.e., the deepest parser) takes precedent if multiple same (e.g., default values) are defined.
