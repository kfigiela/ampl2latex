package pl.edu.agh.mplt


object App {
  def main(args: Array[String]) {
    if (args.size < 1) {
      println("Please specify AMPL file to parse")
    } else {
      val parsedFile = ParsedFile.fromAMPL(args(0))

      println(parsedFile.ast)
    }
  }
}
