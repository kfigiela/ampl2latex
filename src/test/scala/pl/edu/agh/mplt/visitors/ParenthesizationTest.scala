package pl.edu.agh.mplt.visitors

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.visitors.translator.mappers.{AddNecessaryParenthesis, StripAllParenthesis}
import pl.edu.agh.mplt.parser.IntercodeImplicits
import pl.edu.agh.mplt.parser.phrase.expression.{Number, ParenthesizedExpression, Bin}


class ParenthesizationTest extends FlatSpec with Matchers with VisitorImplicits with IntercodeImplicits {
   def mapper = new StripAllParenthesis andThen new AddNecessaryParenthesis

   "fixing parenthesis" should "remove remove redundant parenthesis in expression of same priority" in {
      map(template ("1 + (2 +3) ")).toExpr should be(Bin.+(1, Bin.+(2, 3)))
   }
   it should "strip mested redundant parenthesis" in {
      map (template ("(((1)))")).toExpr should be(Number(1))
   }

   it should "maintain necessary parenthesis of right associative expression's right side " in {
      map(template ("1 - (2 -3) ")).toExpr should be(Bin.-(1, ParenthesizedExpression(Bin.-(2, 3))))
   }

   it should "maintain user specified meaning of expression" in {
      map(template ("(1 + 2) * 3 ^( 5-5)")).toExpr should be(
         Bin.*(ParenthesizedExpression(Bin.+(1, 2)), Bin.^(3, ParenthesizedExpression(Bin.-(5, 5))))
      )
}
}
