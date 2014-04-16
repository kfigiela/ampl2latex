package pl.edu.agh.mplt

import java.io.File


object App {
  def main(args: Array[String]) {
    if (args.size < 1) {
      println("Please specify AMPL file to parse")
    } else {
      val start = System.currentTimeMillis()
      val parsedFile = ParsedFile.fromAMPL(new File(args(0)))
      println("\n\n in: " + (System.currentTimeMillis() - start))

      //      println(parsedFile.ast)

    }
  }
}
