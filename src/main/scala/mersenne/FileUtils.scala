package mersenne

import java.io.{File, PrintWriter}

object FileUtils {
  type FileName = String

  def withFile[A](fileName : FileName)(func : PrintWriter => A) : Unit = {
    val file = new File(fileName)
    val write = new PrintWriter(file)
    try {
      func(write)
    }
    finally {
      write.close()
    }
  }

  def write(fileName : FileName, iterator : Iterator[String]) : Unit = {
    withFile(fileName) { output =>
      iterator.foreach(line => output.println(line))
    }
  }
}
