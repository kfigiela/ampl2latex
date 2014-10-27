package pl.edu.agh.mplt.visitors.translator.mappers


import pl.edu.agh.mplt.parser.phrase.expression._
import pl.edu.agh.mplt.parser.phrase.set._
import pl.edu.agh.mplt.visitors.NodeMapper
import pl.edu.agh.mplt.parser.phrase.logical.{ParenthesizedLogical, LogicalExpression}
import scala.collection.mutable

class ParenthesisStripper(operations: mutable.Buffer[NodeMapper] = mutable.Buffer()) extends NodeMapper(operations) {
   override def mapExpr(expr: Expression): Expression = expr match {
      case ParenthesizedExpression(e) => mapExpr(e)
      case e                          => super.mapExpr(e)
   }

   override def mapSexpr(sexpr: SetExpression): SetExpression = sexpr match {
      case ParenthesizedSetExpression(s) => mapSexpr(s)
      case s                             => super.mapSexpr(s)
   }

   override def mapLexpr(lexpr: LogicalExpression): LogicalExpression = lexpr match {
      case ParenthesizedLogical(l) => mapLexpr(l)
      case l                       => super.mapLexpr(l)
   }
}
