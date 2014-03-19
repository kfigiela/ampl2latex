package pl.edu.agh.mplt.parser.reference

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.formula.Formula
import pl.edu.agh.mplt.parser.formula.logical.Bool
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.formula.set.SetLiteral


sealed trait Reference extends ASTNode with Formula

case class BoolReference(id: String) extends Reference with Bool

case class NumberReference(id: String) extends Reference with Expression

case class SetReference(id: String) extends SetLiteral with Reference