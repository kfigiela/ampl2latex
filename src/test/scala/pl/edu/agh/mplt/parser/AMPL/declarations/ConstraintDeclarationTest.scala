package pl.edu.agh.mplt.parser.AMPL.declarations

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{AMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.phrase.set.{IndexedSet, Indexing}
import pl.edu.agh.mplt.parser.reference.SimpleReference
import pl.edu.agh.mplt.parser.declaration.constraint._
import pl.edu.agh.mplt.parser.declaration.constraint.BoundedConstraint
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration

class ConstraintDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = AMPLParser()

  def expr = parser.constraintDeclaration

  def parse(input: String) = parser.parseAll(expr, input).get

  "Constraint parser" should "parser constraint declaration  without 'subject to' clause" in {
    parse("apples : 1 <= 3;") should be(ConstraintDeclaration("apples",
      constraint = BoundedConstraint(expr = 1, rightExpression = Some(Constraint.<=(3)))))
  }

  it should "parse constraint declaration with 'subject to'  clause" in {
    parse("subject to apples : 1 <= 3;") should be(ConstraintDeclaration("apples",
      constraint = BoundedConstraint(expr = 1, rightExpression = Some(Constraint.<=(3)))))
  }

  it should "parse constraint declaration with an alias" in {
    parse(" apples \"oranges\" : 1 <= 3;") should be(ConstraintDeclaration("apples",
      alias = Some("oranges"),
      constraint = BoundedConstraint(expr = 1, rightExpression = Some(Constraint.<=(3)))))
  }

  it should "parse constraint declaration with '<=' constraint" in {
    parse("apples { i in A} : i <= 3;") should be(ConstraintDeclaration("apples",
      indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
      constraint = BoundedConstraint(expr = "i", rightExpression = Some(Constraint.<=(3)))))
  }


  it should "parse constraint declaration with '=' constraint" in {
    parse("apples { i in A} : i = 3;") should be(ConstraintDeclaration("apples",
      indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
      constraint = BoundedConstraint(expr = "i", rightExpression = Some(Constraint.==((3))))))
  }

  it should "parse constraint declaration with '>=' constraint" in {
    parse("apples { i in A} : i >= 3;") should be(ConstraintDeclaration("apples",
      indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
      constraint = BoundedConstraint(expr = "i", rightExpression = Some(Constraint.>=(3)))))
  }

  it should "parse constraint declaration with '<= x <=' constraint" in {
    parse("apples { i in A} : 1 <= i <=  3;") should be(ConstraintDeclaration("apples",
      indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
      constraint = BoundedConstraint(Some(Constraint.<=(1)), "i", Some(Constraint.<=(3)))))
  }

  it should "parse constraint declaration with '>= x >=' constraint" in {
    parse("apples { i in A} : 1 >= i >= 3;") should be(ConstraintDeclaration("apples",
      indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
      constraint = BoundedConstraint(Some(Constraint.>=(1)), "i", Some(Constraint.>=(3)))))
  }

  it should "parse Mixed complementarity constraint" in {
    parse("apples : 1 complements 2 <= 3 <= 4;") should be(
      ConstraintDeclaration("apples",
        constraint = MixedComplementarity(1,
          BoundedConstraint(Some(Constraint.<=(2)), 3, Some(Constraint.<=(4))))))

    parse("apples : 2 <= 3 <= 4 complements 1;") should be(
      ConstraintDeclaration("apples",
        constraint = MixedComplementarity(1,
          BoundedConstraint(Some(Constraint.<=(2)), 3, Some(Constraint.<=(4))))))
  }

  it should "parse simple complementarity constraint" in {
    parse("apples: 1 <= 2 complements 3 <= 4;") should be(
      ConstraintDeclaration("apples",
        constraint = SimpleComplementarity(
          BoundedConstraint(expr = 1, rightExpression = Some(Constraint.<=(2))),
          BoundedConstraint(expr = 3, rightExpression = Some(Constraint.<=(4))))))
    parse("apples: 1 >= 2 complements 3 >= 4;") should be(
      ConstraintDeclaration("apples",
        constraint = SimpleComplementarity(
          BoundedConstraint(expr = 1, rightExpression = Some(Constraint.>=(2))),
          BoundedConstraint(expr = 3, rightExpression = Some(Constraint.>=(4))))))
  }

  it should "parse example input 1" in {
    parser.parseAll(expr,
      """
        | subject to Fill {i in WIDTHS}:
        | sum {j in PATTERNS} nbr[i,j] * Cut[j] >= orders[i];
      """.stripMargin) match {
      case parser.Success(_, _) =>
      case _                    => throw new Exception
    }
  }
  it should "parse example input 2" in {
    parser.parseAll(expr,
      """
        | subject to Pri_Compl {i in PROD}:
        | Price[i] >= 0 complements
        | sum {j in ACT} io[i,j] * Level[j] >= demand[i];
      """.stripMargin) match {
      case parser.Success(_, _) =>
      case _                    => throw new Exception
    }

  }

}
