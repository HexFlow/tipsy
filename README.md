Tipsy
-----

Tipsy, a tool to parse, analyze, cluster programming MOOC submissions,  in order to tackle the problems such as bias in grading by TAs, helping students by providing relevant tips and suggestions.

# Requirements
* JVM
* sbt
* postgres (optional)
* python2 (optional)
* matplotlib (optional)
* scipy (optional)
* numpy (optional)

Download `sbt` (Scala Build Tool), and place it in your path. It will automatically download the required version of Scala.

For the Python dependencies, you can use a virtualenv in the scripts folder with the above listed dependencies.

You can run postgres using Docker (follow [instructions on their website](https://docs.docker.com/install/linux/docker-ce/ubuntu/) regarding how to install Docker). To do that, run the following:

```bash
docker run -d --name tipsy-db -e POSTGRES_PASSWORD=default -e POSTGRES_USER=default -e POSTGRES_DB=default -p 5432:5432 postgres
```

# Usage

```
bash> sbt
sbt> run help
```

This would display the available commands. For instance, you can run the following to display corrections when comparing two files:
```
sbt> run exec --files <file1>.c,<file2>.c --corrections
```

To see the linear representation of multiple files, you can do either of the following. The second one will run the command on all files in the provided directory.
```
sbt> run exec --linearRep --files <file1>.c,<file2>.c,<file3.c>.....
sbt> run exec dir -n <dir_name> --linearRep
```

# Academic publication

This work has been accepted at the [International Conference on Artificial Intelligence in Education, 2018](https://aied2018.utscic.edu.au) as a poster paper. The full text can be found [on arXiv](https://arxiv.org/abs/1804.00373).

# Code structure

## Documentation
A partial documentation is available, and can be viewed as follows:
```
sbt> doc
cd target/scala-2.12/api
python3 -m http.server
```
This would serve the documentation at `localhost:8000`.

This documentation will be extended in the future.

## Source code
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


## Additional code
In addition, there is some more code in:
* `scripts/`: Python code facilitating program insertion, testing, and clustering.
* `src/main/scala/forcelayout`: Code from [scala-force-layout](https://github.com/rsimon/scala-force-layout) placed here because it has no builds for recent scala versions. Can be ignored.

# TODO
* Provide a requirements.txt file to facilitate a virtualenv in the scripts folder.
