package pl.edu.agh.mplt.parser.AMPL.statements.sexpr

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.formula.set._
import pl.edu.agh.mplt.parser.formula.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.member._
import pl.edu.agh.mplt.parser.{KeywordAMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.reference.ReferenceParser
import pl.edu.agh.mplt.parser.formula.logical.{Comparision, LogicalExpressionAMPLParser}
import pl.edu.agh.mplt.parser.formula.set.SetComprehension
import pl.edu.agh.mplt.parser.formula.expression.Number
import pl.edu.agh.mplt.parser.reference.SimpleReference
import pl.edu.agh.mplt.parser.member.StringMember
import pl.edu.agh.mplt.parser.formula.set.ExplicitSet

class SetExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new ReferenceParser with KeywordAMPLParser with ExpressionAMPLParser with IndexingAMPLParser
    with LogicalExpressionAMPLParser with SetExpressionAMPLParser with MemberAMPLParser

  def expr = parser.sexpr

  def parse(input: String) = parser.parseAll(expr, input).get

  "Set expr parser" should "parse explicit number set definition" in {
    parse("{1, 2, 3}") should be(ExplicitSet(Set[Member](Number(1), Number(2), Number(3))))
  }

  it should "parse reference to set" in {
    parse("a") should be(SimpleReference("a"))
  }

  it should "parse one element set literal" in {
    parse("{ \"a\" }") should be(ExplicitSet(Set[Member](StringMember("a"))))
  }

  it should "parse explicit string literal set definition" in {
    parse("""{"a", "b", "c"}""") should be(ExplicitSet(Set[Member](StringMember("a"), StringMember("b"), StringMember(
      "c"))))
  }

  it should "parse empty set literal" in {
    parse("{}") should be(ExplicitSet(Set.empty))
  }

  it should "parse number set comprehension" in {
    parse("1 .. 10") should be(SetComprehension(1, 10))
  }

  it should "parse string set comprehension" in {
    parse(""" "a" .. "f" """) should be(SetComprehension(StringMember("a"), StringMember("f")))
  }

  it should "parse number set comprehension with step" in {
    parse("1 .. 17 by 5") should be(SetComprehension(1, 17, 5))
  }

  it should "parse string set comprehension with step" in {
    parse(""" "a" .. "d" by 5""") should be(SetComprehension(StringMember("a"), StringMember("d"), 5))
  }

  it should "parse binary union" in {
    parse(" {1, 2, 3} union 1 ..7 by 2") should be(
      Sets.Union(ExplicitSet(Set[Member](1, 2, 3)), SetComprehension(1, 7, 2))
    )
  }

  it should "parse binary inter" in {
    parse(" {1, 2, 3} inter 1 ..7 by 2") should be(
      Sets.Intersection(ExplicitSet(Set[Member](1, 2, 3)), SetComprehension(1, 7, 2))
    )
  }

  it should "parse diff" in {
    parse(" {1, 2, 3} diff 1 ..7 by 2") should be(
      Sets.Difference(ExplicitSet(Set[Member](1, 2, 3)), SetComprehension(1, 7, 2))
    )
  }

  it should "parse symdiff" in {
    parse(" {1, 2, 3} symdiff 1 ..7 by 2") should be(
      Sets.SymetricDifference(ExplicitSet(Set[Member](1, 2, 3)), SetComprehension(1, 7, 2))
    )
  }

  it should "parse cross" in {
    parse(" {1, 2, 3} cross 1 ..7 by 2") should be(
      Sets.Cartesian(ExplicitSet(Set[Member](1, 2, 3)), SetComprehension(1, 7, 2))
    )
  }

  it should "parse comprehension with references" in {
    parse("1 .. a") should be(SetComprehension(1, ExpressionMember("a")))
  }

  it should "parse indexing union" in {
    //        parse(" union {i in A}  1 ..7 by 2")
  }

  it should "parse indexing  inter" in {
    //        parse(" inter {i in A}  1 ..7 by 2")
  }
  ///////////////////////////
  ////// associativity //////
  ///////////////////////////

  it should "maintain left associativity of union" in {
    parse(" {1, 2} union  {1, 2} union {1, 2}") should be(
      Sets.Union(Sets.Union(ExplicitSet(Set[Member](1, 2)), ExplicitSet(Set[Member](1, 2))), ExplicitSet(Set[Member](1,
        2)))
    )
  }

  it should "maintain left associativity of diff" in {
    parse(" {1, 2} diff  {1, 2} diff {1, 2}") should be(
      Sets.Difference(Sets.Difference(ExplicitSet(Set[Member](1, 2)), ExplicitSet(Set[Member](1, 2))), ExplicitSet(
        Set[Member](1, 2)))
    )
  }

  it should "maintain left associativity of symdiff" in {
    parse(" {1, 2}  symdiff {1, 2} symdiff {1, 2}") should be(
      Sets.SymetricDifference(
        Sets.SymetricDifference(ExplicitSet(Set[Member](1, 2)), ExplicitSet(Set[Member](1, 2))),
        ExplicitSet(Set[Member](1, 2)))
    )
  }

  it should "maintain left associativity of intersection" in {
    parse(" {1, 2} inter  {1, 2} inter {1, 2}") should be(
      Sets.Intersection(
        Sets.Intersection(ExplicitSet(Set[Member](1, 2)), ExplicitSet(Set[Member](1, 2))),
        ExplicitSet(Set[Member](1, 2)))
    )
  }

  it should "maintain left associativity of cross" in {
    parse(" {1, 2} cross  {1, 2} cross {1, 2}") should be(
      Sets.Cartesian(
        Sets.Cartesian(ExplicitSet(Set[Member](1, 2)), ExplicitSet(Set[Member](1, 2))),
        ExplicitSet(Set[Member](1, 2)))
    )
  }


  ////////////////////////////
  //////// precedence ////////
  ////////////////////////////

  "intersection" should "precede union" in {
    parse(" {1, 2} union  {1, 2} inter {1, 2}") should be(
      Sets.Union(ExplicitSet(Set[Member](1, 2)), Sets.Intersection(ExplicitSet(Set[Member](1, 2)), ExplicitSet(
        Set[Member](1, 2))))
    )
  }

  it should "precede difference" in {
    parse(" {1, 2} diff  {1, 2} inter {1, 2}") should be(
      Sets.Difference(ExplicitSet(Set[Member](1, 2)), Sets.Intersection(ExplicitSet(Set[Member](1, 2)), ExplicitSet(
        Set[Member](1, 2))))
    )
  }

  it should "precede symetric difference" in {
    parse(" {1, 2} symdiff  {1, 2} inter {1, 2}") should be(
      Sets.SymetricDifference(ExplicitSet(Set[Member](1, 2)), Sets.Intersection(ExplicitSet(Set[Member](1, 2)),
        ExplicitSet(Set[Member](1, 2))))
    )
  }

  "Cartesian product" should "precede intersection" in {
    parse(" {1, 2} inter  {1, 2} cross {1, 2}") should be(
      Sets.Intersection(ExplicitSet(Set[Member](1, 2)), Sets.Cartesian(ExplicitSet(Set[Member](1, 2)), ExplicitSet(
        Set[Member](1, 2))))
    )
  }

  it should "parse conditional expression with else" in {
    parse("if 1 == 1 then {1} else {2}") should be(SetExpressionIf(Comparision.==(1, 1),
      ExplicitSet(Set[Member](1)),
      ExplicitSet(Set[Member](2))))
  }

  it should "parse chained conditional expressions" in {
    parse("if 1 == 1 then if 1 == 1 then {1} else {2} else {3}") should be(
      SetExpressionIf(Comparision.==(Number(1), Number(1)),
        SetExpressionIf(Comparision.==(Number(1), Number(1)),
          ExplicitSet(Set[Member](1)),
          ExplicitSet(Set[Member](2))),
        ExplicitSet(Set[Member](3))))
  }

  it should "parse setof expression" in {
    parse("setof {A} 1") should be(Sets.SetOf(Indexing(List(SimpleReference("A"))), 1))
  }

  it should "parse indexing" in {
    parse("{i in A}") should be(Indexing(List(IndexedSet(List("i"), SimpleReference("A")))))
  }


}
