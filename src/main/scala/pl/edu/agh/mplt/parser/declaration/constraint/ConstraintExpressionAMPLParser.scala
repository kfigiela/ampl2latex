package pl.edu.agh.mplt.parser.declaration.constraint

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.Expression


trait ConstraintExpressionAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def nonKeyword: Parser[String]

  def constraintExpression: Parser[ConstraintExpression] =
    <=<= | >=>= | <= | == | >=

  private def <= = expr ~ "<=" ~ expr ^^ {
    case vexpr ~ _ ~ cexpr => ConstraintExpression(expr = vexpr, rightExpression = Some(Constraint.<=(cexpr)))
  }

  private def == = expr ~ "=" ~ expr ^^ {
    case vexpr ~ _ ~ cexpr => ConstraintExpression(expr = vexpr, rightExpression = Some(Constraint.===(cexpr)))
  }

  private def >= = expr ~ ">=" ~ expr ^^ {
    case vexpr ~ _ ~ cexpr => ConstraintExpression(expr = vexpr, rightExpression = Some(Constraint.>=(cexpr)))
  }

  private def <=<= = expr ~ "<=" ~ expr ~ "<=" ~ expr ^^ {
    case cexpr1 ~ _ ~ vexpr ~ _ ~ cexpr2 =>
      ConstraintExpression(Some(Constraint.<=(cexpr1)), vexpr, Some(Constraint.<=(cexpr2)))
  }

  private def >=>= = expr ~ ">=" ~ expr ~ ">=" ~ expr ^^ {
    case cexpr1 ~ _ ~ vexpr ~ _ ~ cexpr2 =>
      ConstraintExpression(Some(Constraint.>=(cexpr1)), vexpr, Some(Constraint.>=(cexpr2)))
  }

}