package pl.edu.agh.mplt.visitors

import pl.edu.agh.mplt.parser.{ASTNode, AMPLParser}
import pl.edu.agh.mplt.parser.declaration.{InvalidDeclaration, Declaration}
import pl.edu.agh.mplt.parser.phrase.expression.Expression
import pl.edu.agh.mplt.parser.declaration.data.{Attribute, SetDeclaration, ParameterDeclaration, VariableDeclaration}
import pl.edu.agh.mplt.parser.reference.{IndexedReference, SimpleReference}


trait VisitorImplicits {
   def mapper: NodeMapper

   val parser = AMPLParser()

   def template(expr: String) = s"var x = $expr;"

   def parse(input: String): Declaration = parser.parse(input) match {
      case parser.Success(result: Declaration, _) => result
      case msg@parser.Failure(_, _)               => InvalidDeclaration(msg.toString())
      case msg@parser.Error(_, _)                 => InvalidDeclaration(msg.toString())
      case _                                      => throw new Error
   }

   def map(input: String): Declaration = mapper(parse(input))

   implicit class BoxedDeclaration(val dec: ASTNode) {
      def toExpr: Expression = dec match {
         case VariableDeclaration(_, _, _, List(Attribute.FinalValue(expr))) => expr

         case node => throw new Error(s"got $node")
      }

      def toName: String = dec match {
         case VariableDeclaration(name, _, _, _)  => name
         case SetDeclaration(name, _, _, _)       => name
         case ParameterDeclaration(name, _, _, _) => name
         case SimpleReference(name)               => name
         case IndexedReference(ref, _)            => ref.toName

         case node => throw new Error(s"got $node")
      }
   }


}
