package pl.edu.agh.mplt

import scala.io.Source
import java.io.File

class InstructionStream(file: File) {
   private[this] val source: Iterator[Char] = Source.fromFile(file).iter


   private[this] def nextInstruction: Stream[String] = {
      val buffer = new StringBuilder()

      def appendAndThen(char: Char)(f: () => Unit): Unit = {
         buffer.append(char)
         f()
      }

      def appendNormal(): Unit =
         if (source.hasNext) source.next() match {
            case ';'  => buffer.append(';')
            case '<'  => appendAndThen('<')(checkForPiecewise)
            case '#'  => appendComment() //appendAndThen('#')(appendComment)
            case char => appendAndThen(char)(appendNormal)
         }

      def checkForPiecewise(): Unit =
         if (source.hasNext) source.next() match {
            case '<'  => appendAndThen('<')(appendPiecewise)
            case char => appendAndThen(char)(appendNormal)
         }

      /* To also memorize comments for parser simply change '#'
      case in %appendNormal% and generic char case in this method */
      def appendComment(): Unit =
         if (source.hasNext) source.next() match {
            case '\n' => appendAndThen('\n')(appendNormal)
            case char => appendComment() //appendAndThen(char)(appendLine)
         }

      def appendPiecewise(): Unit =
         if (source.hasNext) source.next() match {
            case '>'  => appendAndThen('>')(checkForEndOfPiecewise)
            case char => appendAndThen(char)(appendPiecewise)
         }

      def checkForEndOfPiecewise(): Unit =
         if (source.hasNext) source.next() match {
            case '>'  => appendAndThen('>')(appendNormal)
            case char => appendAndThen(char)(appendPiecewise)
         }

      if (source.hasNext) appendNormal()

      if (buffer.isEmpty) Stream.empty
      else Stream.cons(buffer.toString().replaceAll( """(?m)\s+$""", ""), nextInstruction)
   }


   lazy val instructions: Stream[String] = nextInstruction.filter(_.length > 0)

}
