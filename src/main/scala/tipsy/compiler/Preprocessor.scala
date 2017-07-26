package tipsy.compiler

import sys.process._
import java.io.{ByteArrayOutputStream, PrintWriter}

object Preprocessor {
  def runCommand(cmd: Seq[String]): (Int, String, String) = {
    val stdoutSt = new ByteArrayOutputStream
    val stderrSt = new ByteArrayOutputStream
    val stdoutWr = new PrintWriter(stdoutSt)
    val stderrWr = new PrintWriter(stderrSt)
    val exitValue =
      cmd.!(ProcessLogger(stdoutWr.println, stderrWr.println))
    stdoutWr.close()
    stderrWr.close()
    (exitValue, stdoutSt.toString, stderrSt.toString)
  }

  def apply(filename: String): Either[CPreError, String] = {
    val (ret, out, err) = runCommand(Seq("gcc", "-E", filename))
    if (ret != 0) {
      println("Error running preprocessor: " + ret)
      println(err)
      Left(CPreError(err))
    } else {
      var flag = false
      // Contains the part of user code from GCC output
      val tresult = out.split("\n").collect {
        case line if flag && !line.startsWith("#") => Some(line)
        case line if line.startsWith("#") => {
          if (line.contains(filename)) flag = true
          else flag = false
          None
        }
      }.collect{ case Some(x) => x }
      var result: String = "";
      if (tresult.length != 0) {
        result = tresult.reduceLeft(_ + "\n" + _)
      }
      Right(result)
    }
  }
}
