package pl.edu.agh.mplt.parser.declaration.constraint

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.declaration.PiecewiseLinearTerm


trait ConstraintDeclarationAMPLParser extends JavaTokenParsers {
  def nonKeyword: Parser[String]

  def indexing: Parser[Indexing]

  def constraintExpression: Parser[ConstraintExpression]

  def piecewiseLinearTerm: Parser[PiecewiseLinearTerm]

  def constraintDeclaration: Parser[ConstraintDeclaration] =
    ("subject to" ?) ~> nonKeyword ~ (nonKeyword ?) ~ (indexing ?) ~ ":" ~ constraintExpression ~
    (piecewiseLinearTerm ?) <~ ";" ^? { case name ~ optAlias ~ optIndexing ~ _ ~ constraint ~ piecewiseOpt =>
      ConstraintDeclaration(name, optAlias, optIndexing, constraint, piecewiseOpt)
    }
}
