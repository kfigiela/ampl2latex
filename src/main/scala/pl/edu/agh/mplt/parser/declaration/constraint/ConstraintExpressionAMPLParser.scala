package pl.edu.agh.mplt.parser.declaration.constraint

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.Expression


trait ConstraintExpressionAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def keyword: Parser[String]

  def constraintExpression: Parser[ConstraintExpression] =
    complementary | boundedConstraint

  private def boundedConstraint = dualBounds | singleBound

  private def complementary = dualBounds ~ keyword ~ expr ^? {
    case cexpr ~ "complements" ~ expr => MixedComplementarity(expr, cexpr)
  } | expr ~ keyword ~ dualBounds ^? {
    case expr ~ "complements" ~ cexpr => MixedComplementarity(expr, cexpr)
  } | singleBound ~ keyword ~ singleBound ^? {
    case leftBound ~ "complements" ~ rightBound => SimpleComplementarity(leftBound, rightBound)
  }

  private def singleBound = <= | == | >=

  private def <= = expr ~ "<=" ~ expr ^^ {
    case vexpr ~ _ ~ cexpr => BoundedConstraint(expr = vexpr, rightExpression = Some(Constraint.<=(cexpr)))
  }

  private def == = expr ~ "=" ~ expr ^^ {
    case vexpr ~ _ ~ cexpr => BoundedConstraint(expr = vexpr, rightExpression = Some(Constraint.===(cexpr)))
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