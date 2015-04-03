package pl.edu.agh.mplt

import java.io.{PrintWriter, File}

import scala.reflect.io.Path
import scala.io.Source


object App {
  def main(args: Array[String]) {
    if (args.size < 1) {
      println("Please specify input")
      System.exit(1)
    }

    val prefix = Path(args(0)).name.split('.').init.mkString(".")

    val parsedFile = Translator.fromAMPL(new File(args(0)))

    val mainName = s"$prefix.tex"

    if(!(new File(mainName)).exists) {
      val out = new PrintWriter(new File(mainName))
      try {
        val template = Source.fromURL(getClass.getResource("/template.tex")).mkString
        val content = template.replaceAll("\\$prefix", prefix)
        out.print(content)
      } finally {
        out.close()
      }
    }

    parsedFile.index.foreach({
      case(name, ds) => {
        val out = new PrintWriter(new File(s"$prefix.$name.index.tex"))
        try {
          out.print(ds)
        } finally {
          out.close()
        }
      }
    })

    parsedFile.translate.foreach({
      case(name, ds) => {
        val out = new PrintWriter(new File(s"$prefix.$name.formulas.tex"))
        try {
          out.print(ds)
        } finally {
          out.close()
        }
      }
    })

    if (parsedFile.ast.errors.nonEmpty) {
      parsedFile.ast.printErrors()
      System.exit(1)
    } else {
      println("OK")
    }
//    } catch {
//      case e: Throwable =>
//        println(e.getMessage)
//        out.write("\n error: " + e.getMessage)
//        out.close()
//        System.exit(1)

    System.exit(0)
  }
}
