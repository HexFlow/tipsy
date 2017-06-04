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
sbt> run -<arg> <files>
```

Where arg can be as follows:

| arg       | meaning          |
| --------- | ---------------- |
| pp        | PRINTPARSE       |
| pf        | PRINTFLOW        |
| dp        | DRAWPARSE        |
| df        | DRAWFLOW         |
| le        | LEASTEDIT        |
| len=int   | LEASTEDITLIMIT   |
| web       | CLI(default)/WEB |

`dp` and `df` will generate an image in the project folder for each file specified.

## Notes
Code for Undergraduate project by [Pallav Agarwal](https://github.com/pallavagarwal07) and [Saksham Sharma](https://github.com/sakshamsharma).
