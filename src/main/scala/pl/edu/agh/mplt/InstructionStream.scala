package pl.edu.agh.mplt

import java.io.{FileWriter, FileOutputStream, File}

import scala.collection.mutable
import scala.io.Source

class InstructionStream(file: File) {
  private[this] val source: Iterator[Char] = Source.fromFile(file).iter

  private[this] def nextInstruction(in: Seq[Char]): Stream[String] = {
    implicit val buffer = mutable.Buffer[Char]()
    val tail = preprocess(in)

    val res = buffer.reverse.mkString.replaceAll("""[ \t]+""", " ").replaceAll( """(?m)\s+$""", "")

    if (buffer.isEmpty) Stream.empty
    else Stream.cons(res, nextInstruction(tail).filter(!_.isEmpty))
  }

  lazy val instructions: Stream[String] = nextInstruction(source.toStream)

  private[this] def preprocess(input: Seq[Char])(implicit acc: mutable.Buffer[Char]): Seq[Char] =    input match {
    case Nil => Nil
    case '<' +: '<' +: tail => append('<', 2); jumpOverPiecewise(tail)
    case '#' +: tail => jumpOverComment(tail)
    case ';' +: tail => append(';'); tail
    case 13 +: 10 +: tail => append(' '); preprocess(tail)
    case 10 +: tail => append(' '); preprocess(tail)
    case c +: tail => append(c);preprocess(tail)
  }

  private[this] def jumpOverPiecewise(input: Seq[Char])(implicit acc: mutable.Buffer[Char]): Seq[Char] = input match {
    case '#' +: tail => jumpOverComment(tail)
    case '>' +: '>' +: tail => append(">>"); preprocess(tail)
    case 13 +: 10 +: tail => append(' '); jumpOverPiecewise(tail)
    case 10 +: tail => append(' '); jumpOverPiecewise(tail)
    case c +: tail => append(c);jumpOverPiecewise(tail);
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
