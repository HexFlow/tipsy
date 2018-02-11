Tipsy
-----

Tipsy, a tool to parse, analyze, cluster programming MOOC submissions,  in order to tackle the problems such as bias in grading by TAs, helping students by providing relevant tips and suggestions.

[Sample parse tree](https://raw.githubusercontent.com/sakshamsharma/tipsy/master/sample.png)

## Requirements
* JVM
* sbt
* graphviz/dot (optional, for graphical parse tree)

That's it. You just need to download `sbt` (Scala Build Tool), and place it in your path. It will automatically download the required version of Scala.

## Using
```
bash> sbt
sbt> run help
```

It should give you the following output:

```
Command: exec [dir] [options]
whether to run operations on a given set of input programs
  --files <file1>,<file2>...
                           files to run analysis on
  --ids <id1>,<id2>...     program IDs to run analysis on
  -d, --distance           whether to print distance between programs
  -c, --corrections        whether to print corrections
  --parseTree              whether to show ParseTree
  --linearRep              whether to show LinearRepresentation
  --normalRep              whether to show Normalized Linear Representation
Command: exec dir [options]
directory to run analysis on
  -n, --names <value>
  -l, --limit <value>      limit on programs to analyse
Command: cluster [options]
whether to do cluster related operations
  -q, --ques <value>       question whose cluster to run analysis on
  -u, --update             whether to update cluster database
  -v, --variance           whether to print cluster variance
  -dm, --dumpmatrix        whether to dump distance matrix
Command: web [options]
whether to serve as a web backend
  --host <value>           address to listen on
  --port <value>           port to listen on
```

## Notes
Code for Undergraduate project by [Pallav Agarwal](https://github.com/pallavagarwal07) and [Saksham Sharma](https://github.com/sakshamsharma).
