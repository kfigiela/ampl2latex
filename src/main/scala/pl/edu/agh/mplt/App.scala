package pl.edu.agh.mplt

import java.io.{PrintWriter, File}
import scala.annotation.tailrec

object App {
   def main(args: Array[String]) {
      if (args.size < 2) {
         println("Please specify input and output files")
      } else {
         val start = System.currentTimeMillis()
         val parsedFile = ParsedFile.fromAMPL(new File(args(0)))

         val out = new PrintWriter(new File(args(1)))

         println("starts")

         try {
            @tailrec
            def persist(stream: Stream[String]): Unit = if (!stream.isEmpty) {
               out.println(s"${stream.head } \\\\")
               persist(stream.tail)
            }

            println("prints")
            persist(parsedFile.translate)

         } catch {
            case e: Throwable =>
               out.write("\n error: " + e.getMessage)
               throw e
         } finally {
            out.close()
         }

         println("\n\n in: " + (System.currentTimeMillis() - start))
      }
   }
}
