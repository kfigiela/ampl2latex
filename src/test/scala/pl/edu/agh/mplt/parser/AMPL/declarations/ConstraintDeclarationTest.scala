package pl.edu.agh.mplt.parser.AMPL.declarations

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{AMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.formula.set.{IndexedSet, Indexing}
import pl.edu.agh.mplt.parser.reference.SimpleReference
import pl.edu.agh.mplt.parser.declaration.constraint._
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintExpression
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration

class ConstraintDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = AMPLParser()

  def expr = parser.constraintDeclaration

  def parse(input: String) = parser.parse(expr, input).get

  "Constraint parser" should "parser constraint declaration  without 'subject to' clause" in {
    parse("apples : 1 <= 3;") should be(ConstraintDeclaration("apples",
      constraint = ConstraintExpression(expr = 1, rightExpression = Some(Constraint.<=(3)))))
  }

  it should "parse constraint declaration with 'subject to'  clause" in {
    parse("subject to apples : 1 <= 3;") should be(ConstraintDeclaration("apples",
      constraint = ConstraintExpression(expr = 1, rightExpression = Some(Constraint.<=(3)))))
  }

  it should "parse constraint declaration with an alias" in {
    parse(" apples oranges : 1 <= 3;") should be(ConstraintDeclaration("apples",
      alias = Some("oranges"),
      constraint = ConstraintExpression(expr = 1, rightExpression = Some(Constraint.<=(3)))))
  }

  it should "parse constraint declaration with '<=' constraint" in {
    parse("apples { i in A} : i <= 3;") should be(ConstraintDeclaration("apples",
      indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
      constraint = ConstraintExpression(expr = "i", rightExpression = Some(Constraint.<=(3)))))
  }


  it should "parse constraint declaration with '=' constraint" in {
    parse("apples { i in A} : i = 3;") should be(ConstraintDeclaration("apples",
      indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
      constraint = ConstraintExpression(expr = "i", rightExpression = Some(Constraint.===((3))))))
  }

  it should "parse constraint declaration with '>=' constraint" in {
    parse("apples { i in A} : i >= 3;") should be(ConstraintDeclaration("apples",
      indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
      constraint = ConstraintExpression(expr = "i", rightExpression = Some(Constraint.>=(3)))))
  }

  it should "parse constraint declaration with '<= x <=' constraint" in {
    parse("apples { i in A} : 1 <= i <=  3;") should be(ConstraintDeclaration("apples",
      indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
      constraint = ConstraintExpression(Some(Constraint.<=(1)), "i", Some(Constraint.<=(3)))))
  }

  it should "parse constraint declaration with '>= x >=' constraint" in {
    parse("apples { i in A} : 1 >= i >= 3;") should be(ConstraintDeclaration("apples",
      indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
      constraint = ConstraintExpression(Some(Constraint.>=(1)), "i", Some(Constraint.>=(3)))))
  }

}
