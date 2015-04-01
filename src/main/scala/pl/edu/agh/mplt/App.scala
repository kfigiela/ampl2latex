package pl.edu.agh.mplt

import java.io.{PrintWriter, File}

import scala.reflect.io.Path

object App {
  def main(args: Array[String]) {
    if (args.size < 1) {
      println("Please specify input")
      System.exit(1)
    }

    val prefix = Path(args(0)).name.split('.').init.mkString(".")

    val parsedFile = Translator.fromAMPL(new File(args(0)))
    val out = new PrintWriter(new File(s"$prefix.formulas.tex"))

    try {
      parsedFile.translate.foreach(out.print)

      parsedFile.index.foreach({
        case(name, ds) => {
          val ds = parsedFile.index.get(name).get
          val out = new PrintWriter(new File(s"$prefix.$name.tex"))
          out.print(ds)
          out.close()
        }
      })

      if (parsedFile.ast.errors.nonEmpty) {
        parsedFile.ast.printErrors()
        out.close()
        System.exit(1)
      }
    } catch {
      case e: Throwable =>
        out.write("\n error: " + e.getMessage)
        out.close()
        System.exit(1)
    } finally out.close()

    System.exit(0)
  }
}
