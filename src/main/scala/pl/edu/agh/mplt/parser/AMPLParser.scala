package pl.edu.agh.mplt.parser

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.declaration.set.{SetAttributesAMPLParser, SetDeclarationAMPLParser, SetDeclaration}
import pl.edu.agh.mplt.parser.declaration.param.{ParameterAttributesAMPLParser, ParameterDeclarationAMPLParser, ParameterDeclaration}
import pl.edu.agh.mplt.parser.declaration.variable.{VariableAttributesAMPLParser, VariableDeclarationAMPLParser, VariableDeclaration}
import pl.edu.agh.mplt.parser.declaration.constraint.{ConstraintExpressionAMPLParser, ConstraintDeclarationAMPLParser, ConstraintDeclaration}
import pl.edu.agh.mplt.parser.declaration.objective.{ObjectiveDeclarationAMPLParser, ObjectiveDeclaration}
import pl.edu.agh.mplt.parser.formula.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.set.{IndexingAMPLParser, SetExpressionAMPLParser}
import pl.edu.agh.mplt.parser.member.MemberAMPLParser
import pl.edu.agh.mplt.parser.reference.ReferenceParser

trait AMPLParser extends JavaTokenParsers {
  def setDeclaration: Parser[SetDeclaration]

  def parameterDeclaration: Parser[ParameterDeclaration]

  def variableDeclaration: Parser[VariableDeclaration]

  def constraintDeclaration: Parser[ConstraintDeclaration]

  def objectiveDeclaration: Parser[ObjectiveDeclaration]

  def file = rep1(declaration)

  private def declaration: Parser[Declaration] =
    setDeclaration | parameterDeclaration | variableDeclaration | constraintDeclaration | objectiveDeclaration
}

object AMPLParser {
  def apply(): AMPLParser = new AMPLParser
    with SetDeclarationAMPLParser with SetAttributesAMPLParser
    with ParameterDeclarationAMPLParser with ParameterAttributesAMPLParser
    with VariableDeclarationAMPLParser with VariableAttributesAMPLParser
    with ConstraintDeclarationAMPLParser with ConstraintExpressionAMPLParser
    with ObjectiveDeclarationAMPLParser
    with ExpressionAMPLParser
    with LogicalExpressionAMPLParser
    with SetExpressionAMPLParser with IndexingAMPLParser
    with MemberAMPLParser
    with ReferenceParser
    with KeywordAMPLParser
}
