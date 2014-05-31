package pl.edu.agh.mplt.parser

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.declaration.constraint.{ConstraintExpressionAMPLParser,
ConstraintDeclarationAMPLParser, ConstraintDeclaration}
import pl.edu.agh.mplt.parser.declaration.objective.{ObjectiveDeclarationAMPLParser, ObjectiveDeclaration}
import pl.edu.agh.mplt.parser.phrase.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.phrase.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.phrase.set.{IndexingAMPLParser, SetExpressionAMPLParser}
import pl.edu.agh.mplt.parser.member.MemberAMPLParser
import pl.edu.agh.mplt.parser.reference.ReferenceParser
import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.declaration.assertion.{Assertion, CheckAMPLParser}
import pl.edu.agh.mplt.parser.declaration.data.{DataDeclaration, AttributeAMPLParser,
DatatypeDeclarationAMPLParser}

class AMPLParser extends JavaTokenParsers with CheckAMPLParser with KeywordAMPLParser with CommentAMPLParser
                         with DatatypeDeclarationAMPLParser with AttributeAMPLParser
                         with ConstraintDeclarationAMPLParser with ConstraintExpressionAMPLParser
                         with ObjectiveDeclarationAMPLParser with ExpressionAMPLParser with LogicalExpressionAMPLParser
                         with SetExpressionAMPLParser with IndexingAMPLParser with MemberAMPLParser
                         with ReferenceParser {
   def parse(input: String): ParseResult[Declaration] = parseAll(declaration, input)

   private def declaration: Parser[Declaration] =
      datatypeDeclaration | constraintDeclaration | objectiveDeclaration | assertion

}

object AMPLParser {
   def apply(): AMPLParser = new AMPLParser()

}
