package pl.edu.agh.mplt.visitors.indexer

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.declaration.InvalidDeclaration
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration
import pl.edu.agh.mplt.parser.declaration.data.DataDeclaration
import pl.edu.agh.mplt.parser.declaration.objective.ObjectiveDeclaration
import pl.edu.agh.mplt.visitors.translator.Translator
import pl.edu.agh.mplt.visitors.Visitor


class ReferenceIndexer extends Translator[Declaration] {
  def defineEntry(group: String, name: String) = s"\\newglossaryentry{${name}} {\n" +
    s"    type=$group,\n" +
    s"    name={\\ensuremath{${name.replace("_", "\\_")}}},\n" +
    s"    symbol={\\ensuremath{${name.replace("_", "\\_")}}},\n" +
    s"    description={TODO}\n" +
    s"}"
  override def apply(node: Declaration): String = node match {
    case a: Assertion             => ""
    case d: DataDeclaration       => defineEntry("param", d.name)
    case o: ObjectiveDeclaration  => defineEntry("objective", o.name)
    case c: ConstraintDeclaration => ""

    case InvalidDeclaration(msg) => ""

    case dec => throw new Error(s"Unexpected token: $dec")
  }

}
