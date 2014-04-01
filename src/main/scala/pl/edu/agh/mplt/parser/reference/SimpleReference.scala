package pl.edu.agh.mplt.parser.reference

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.formula.logical.Bool
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.formula.set.SetLiteral

trait Reference extends ASTNode with Bool with Expression with SetLiteral

case class SimpleReference(id: String) extends Reference

case class IndexedReference(ref: Reference, index: Expression) extends Reference