package tipsy.compiler

import scala.sys.process._
import java.io.{ByteArrayOutputStream, PrintWriter, File}

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

  def gcc(filename: String): Either[CPreError, String] = {
    val (ret, out, err) = runCommand(Seq("gcc", "-E", filename))
    if (ret != 0) {
      println("Error running preprocessor: " + ret)
      println(err)
      Left(CPreError(err))
    } else {
      // Extract the part of user code from GCC output.
      // Folding accumulator: (Lines collected till now, in reverse for efficient appends to list).
      // The List.Fill part is to ensure that the line numbers in this new program output (after preprocessing)
      // are the same as in the original program. This allows us to display tips on the correct line at
      // later stages.
      val tresult = out.split("\n").foldLeft { List("") } { (collected, curline) =>
        if (curline.startsWith("#") && curline.contains(filename)) {
          // Parse the line number from the pattern, and add as many empty strings.
          val pattern = "# ([0-9]+).*".r
          val pattern(prevLineCount) = curline
          List.fill(prevLineCount.toInt-1)("")
        } else {
          curline :: collected
        }
      }.reverse

      if (tresult.length != 0) {
        Right(tresult.reduceLeft(_ + "\n" + _))
      } else {
        Right("")
      }
    }
  }

  def clangFormat(filename: String): Either[CPreError, String] = {
    val newfile = Compiler.getFilename()
    ("clang-format" #< new File(filename) #> new File(newfile)).!
    Right(newfile)
  }
}
