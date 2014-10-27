package pl.edu.agh.mplt.parser.reference

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.phrase.logical.LogicalExpression
import pl.edu.agh.mplt.parser.phrase.expression.Expression
import pl.edu.agh.mplt.parser.phrase.set.SetLiteral

sealed trait Reference extends ASTNode with LogicalExpression with Expression with SetLiteral

case class SimpleReference(id: String) extends Reference

case class IndexedReference(ref: Reference, index: List[Expression]) extends Reference