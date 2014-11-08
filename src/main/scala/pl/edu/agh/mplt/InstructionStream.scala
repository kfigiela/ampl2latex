package pl.edu.agh.mplt

import java.io.{FileWriter, FileOutputStream, File}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

class InstructionStream(file: File) {
  private[this] val source: Iterator[Char] = Source.fromFile(file).iter

  private[this] def nextInstruction(in: Seq[Char]): Stream[String] = {
    implicit val buffer = mutable.Buffer[Char]()
    val tail = process(in)

    val res = buffer.mkString.reverse.replaceAll("""[ \t]+""", " ").replaceAll( """(?m)\s+$""", "")

    if (buffer.isEmpty) Stream.empty
    else Stream.cons(res, nextInstruction(tail).filter(!_.isEmpty))
  }

  lazy val instructions: Stream[String] = nextInstruction(source.toStream)

  @tailrec
  private[this] def process(input: Seq[Char])(implicit acc: mutable.Buffer[Char]): Seq[Char] =    input match {
    case Nil => Nil
    case '<' +: '<' +: tail => append('<', 2); processPiecewise(tail)
    case '#' +: tail => process(jumpOverComment(tail))
    case ';' +: tail => append(';'); tail
    case 13 +: 10 +: tail => append(' '); process(tail)
    case 10 +: tail => append(' '); process(tail)
    case c +: tail => append(c);process(tail)
  }

  @tailrec
  private[this] def processPiecewise(input: Seq[Char])(implicit acc: mutable.Buffer[Char]): Seq[Char] = input match {
    case '#' +: tail => processPiecewise(jumpOverComment(tail))
    case '>' +: '>' +: tail => append(">>"); process(tail)
    case 13 +: 10 +: tail => append(' '); processPiecewise(tail)
    case 10 +: tail => append(' '); processPiecewise(tail)
    case c +: tail => append(c);processPiecewise(tail);
    case Nil => input
  }


  private[this] def jumpOverComment(input: Seq[Char])(implicit acc: mutable.Buffer[Char]): Seq[Char] = {
    var t = input
    while (t.nonEmpty && t.head != '\n') t = t.tail
    if (t.isEmpty) t else t.tail
  }

  def append(c: Char)(implicit chars: mutable.Buffer[Char]): Unit = {
    c +=: chars
  }

  def append(cs: Seq[Char])(implicit chars: mutable.Buffer[Char]): Unit = {
    cs ++=: chars
  }

  def append(c: Char, times: Int)(implicit chars: mutable.Buffer[Char]): Unit = {
    (1 to times) foreach (_ => c +=: chars)
  }

}
