package pl.edu.agh.mplt.parser.AMPL

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{AMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.declaration.set.SetDeclaration
import pl.edu.agh.mplt.parser.formula.set.{Indexing, IndexedSet}
import pl.edu.agh.mplt.parser.reference.SimpleReference


class CommentsTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = AMPLParser()

  def expr = parser.setDeclaration

  def parse(input: String) = parser.parseAll(expr, input)


  "Comment parser" should "parse simple comments" in {
    parse(
      """ # Here goes comment
        | set # some meaningless comment
        | x {i # never expected a comment in here?
        | in A #surprise
        | }
        | ; #Another one goes here
        | #This is the las one
        | ## except for this one""".stripMargin).get should be(SetDeclaration("x",
      indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A")))))))
  }

}
