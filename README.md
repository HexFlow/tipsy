Tipsy
-----

Tipsy, a tool to parse, analyze, cluster programming MOOC submissions,  in order to tackle the problems such as bias in grading by TAs, helping students by providing relevant tips and suggestions.

## Requirements
* JVM
* sbt
* python2
* matplotlib
* scipy
* numpy

That's it. Download `sbt` (Scala Build Tool), and place it in your path. It will automatically download the required version of Scala.

For the Python dependencies, you can use a virtualenv in the scripts folder with the above listed dependencies.

## Usage
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

## Code structure

The project's source code is structured into the following folders inside `src/main/scala/tipsy/`:
* actor
* compare
* compiler
* db
* frontend
* lexer
* parser

The actual execution starts from the file `src/main/scala/tipsy/Tipsy.scala`. The various folders of the project are described here:

### frontend
Contains the code to allow interaction of this software with the user. We currently allow CLI and a Web-server based interactions. The file `frontend/CLI.scala` contains the entrypoint of this file, and it decides whether to launch the web server, or whether to execute actions from the CLI.

### compiler
Contains functions which launch gcc preprocessor, and the lexer/parser provided by Tipsy, to compile input programs to the internal representation of Tipsy.

### lexer
Contains the lexer tokens used by our parsing process, and the lexer code which outputs those tokens, given an input C program.

### parser
Contains the parser code which converts input C programs to parse trees, and later into linear representations.

### compare
Contains the program comparision logic, cluster actions, normalization functions, et cetera. Detailed description of these files is incoming.

### db
Contains the database models (using Slick ORM), and helper functions for database actions. Also contains serialization functions which convert internal types to database-friendly formats.

### actor
Contains Akka actors which run as separate threads. They execute various asynchronous tasks on the database (for instance, inserting new programs, updating the clusters, et cetera). Some of these tasks have to be executed sequentially to avoid race conditions (for example: Updating distances on inserting a new program). Thus, only one actor is spawned for those tasks, and that actor can only use one thread of execution.



In addition, there is some more code in:
* `scripts/`: Python code facilitating program insertion, testing, and clustering.
* `src/main/scala/forcelayout`: Code from [scala-force-layout](https://github.com/rsimon/scala-force-layout) placed here because it has no builds for recent scala versions. Can be ignored.


## TODO
* Provide a requirements.txt file to facilitate a virtualenv in the scripts folder.
