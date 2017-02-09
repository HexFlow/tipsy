Tipsy
-----

Tipsy is a mini-compiler written in Scala using recursive descent parsing (Packrat). The intention is to be able to identify errors in MOOC course submissions in an automated way, classify them, and aid in correction.

[Sample parse tree](https://raw.githubusercontent.com/sakshamsharma/tipsy/master/sample.png)

## Requirements
* JVM
* sbt
* graphviz/dot (optional, for graphical parse tree)

That's it. You just need to download `sbt` (Scala Build Tool), and place it in your path. It will automatically download the required version of Scala.

## Using
```
bash> sbt
sbt> run test/C2.c
```

This will generate an image `parsetree.png` in your project folder, for the file test/C2.c
