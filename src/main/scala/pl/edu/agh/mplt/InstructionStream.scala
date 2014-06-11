package pl.edu.agh.mplt

import scala.io.Source
import java.io.File


class InstructionStream(filename: File) {

   private[this] val source: Iterator[String] = Source.fromFile(filename).getLines()
   private[this] var carry                    = ""

   private[this] def getStream: Stream[String] = source.toStream match {
      case stream if stream.isEmpty => Stream.empty[String]
      case stream                   =>
         val line = stream.head
         val (instructions, newCarry) =
            if (line.endsWith(";")) {
               // >= 1 lines without carry
               (((carry ++ line) split ";").map(_ + ";"), "")
            } else {
               // carry exists
               val s = (carry ++ line) split ";"
               (s.dropRight(1).map(_ + ";"), s.last + "\n")
            }
         carry = newCarry


         appendToStream(instructions.toStream)
   }

   def appendToStream(stream: Stream[String]): Stream[String] = stream match {
      case s if s.isEmpty => getStream
      case hd #:: tl      => Stream.cons(hd, appendToStream(tl))
   }

   lazy val instructions: Stream[String] = getStream


}

object Test {
   val r1 = """<<.*;.*>>""".r
   val r2 = """.*>>.*;""".r

   val input =
      "minimize something<<limit1[i,j], limit2[i,j];rate1[i,j], rate2[i,j], rate3[i,j]>> Trans[i,j];"

   def main(args: Array[String]) {
      val s = r1 findFirstIn input
      val i = s.get.length

      println("index: " , input.indexOf("<<"))

      println(s, i, input.drop(i))

      val e = r2 findFirstIn input.drop(i)

      println(s"|${input.take(input.indexOf("<<"))} ${s.get} + ${e.get}|")
   }
}