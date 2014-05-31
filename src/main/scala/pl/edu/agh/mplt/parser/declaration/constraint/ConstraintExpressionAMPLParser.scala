package pl.edu.agh.mplt.parser.declaration.constraint

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.phrase.expression.Expression


trait ConstraintExpressionAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def constraintExpression: Parser[ConstraintExpression] =
    complementary | boundedConstraint

  private def boundedConstraint = dualBounds | singleBound

  private def complementary = dualBounds ~ "complements" ~ expr ^? {
    case cexpr ~ _ ~ expr => MixedComplementarity(expr, cexpr)
  } | expr ~ "complements" ~ dualBounds ^? {
    case expr ~ _ ~ cexpr => MixedComplementarity(expr, cexpr)
  } | singleBound ~ "complements" ~ singleBound ^? {
    case leftBound ~ _ ~ rightBound => SimpleComplementarity(leftBound, rightBound)
  }

  private def singleBound = <= | == | >=

  private def <= = expr ~ "<=" ~ expr ^^ {
    case vexpr ~ _ ~ cexpr => BoundedConstraint(expr = vexpr, rightExpression = Some(Constraint.<=(cexpr)))
  }

  private def == = expr ~ "=" ~ expr ^^ {
    case vexpr ~ _ ~ cexpr => BoundedConstraint(expr = vexpr, rightExpression = Some(Constraint.==(cexpr)))
  }

  private def >= = expr ~ ">=" ~ expr ^^ {
    case vexpr ~ _ ~ cexpr => BoundedConstraint(expr = vexpr, rightExpression = Some(Constraint.>=(cexpr)))
  }

  private def dualBounds = <=<= | >=>=

  private def <=<= = expr ~ "<=" ~ expr ~ "<=" ~ expr ^^ {
    case cexpr1 ~ _ ~ vexpr ~ _ ~ cexpr2 =>
      BoundedConstraint(Some(Constraint.<=(cexpr1)), vexpr, Some(Constraint.<=(cexpr2)))
  }

  private def >=>= = expr ~ ">=" ~ expr ~ ">=" ~ expr ^^ {
    case cexpr1 ~ _ ~ vexpr ~ _ ~ cexpr2 =>
      BoundedConstraint(Some(Constraint.>=(cexpr1)), vexpr, Some(Constraint.>=(cexpr2)))
  }


}