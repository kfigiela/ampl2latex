package pl.edu.agh.mplt.parser

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.declaration.set.{SetAttributesAMPLParser, SetDeclarationAMPLParser, SetDeclaration}
import pl.edu.agh.mplt.parser.declaration.param.{ParameterAttributesAMPLParser, ParameterDeclarationAMPLParser,
ParameterDeclaration}
import pl.edu.agh.mplt.parser.declaration.variable.{VariableAttributesAMPLParser, VariableDeclarationAMPLParser,
VariableDeclaration}
import pl.edu.agh.mplt.parser.declaration.constraint.{ConstraintExpressionAMPLParser,
ConstraintDeclarationAMPLParser, ConstraintDeclaration}
import pl.edu.agh.mplt.parser.declaration.objective.{ObjectiveDeclarationAMPLParser, ObjectiveDeclaration}
import pl.edu.agh.mplt.parser.formula.expression.{Expression, ExpressionAMPLParser}
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.set.{Indexing, IndexingAMPLParser, SetExpressionAMPLParser}
import pl.edu.agh.mplt.parser.member.MemberAMPLParser
import pl.edu.agh.mplt.parser.reference.ReferenceParser
import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.declaration.assertion.{Assertion, CheckAMPLParser}

trait AMPLParser extends JavaTokenParsers {
  def fileOpening: Parser[String] = "problem" ~> nonKeyword <~ ";"

  def nonKeyword: Parser[String]

  def setDeclaration: Parser[SetDeclaration]

  def parameterDeclaration: Parser[ParameterDeclaration]

  def variableDeclaration: Parser[VariableDeclaration]

  def constraintDeclaration: Parser[ConstraintDeclaration]

  def objectiveDeclaration: Parser[ObjectiveDeclaration]

  def check: Parser[Assertion]

  def parse(input: String): ParseResult[List[Declaration]] = parseAll(declarations, input)

  def declarations = (fileOpening ?) ~> rep1(declaration)

  def pointsAndSlopes: Parser[(List[(Option[Indexing], Expression)], List[(Option[Indexing], Expression)])]

  def exprs: Parser[List[(Option[Indexing], Expression)]]

  private def declaration: Parser[Declaration] =
    setDeclaration | parameterDeclaration | variableDeclaration | constraintDeclaration | objectiveDeclaration | check

}

object AMPLParser {
  def apply(): AMPLParser = new AMPLParser with KeywordAMPLParser with CommentAMPLParser
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
    with CheckAMPLParser

}
