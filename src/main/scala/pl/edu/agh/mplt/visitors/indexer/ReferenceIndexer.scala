package pl.edu.agh.mplt.visitors.indexer

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.declaration.InvalidDeclaration
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration
import pl.edu.agh.mplt.parser.declaration.data.{SetDeclaration, VariableDeclaration, ParameterDeclaration, DataDeclaration}
import pl.edu.agh.mplt.parser.declaration.objective.ObjectiveDeclaration
import pl.edu.agh.mplt.parser.phrase.set.{IndexedSet, Indexing}
import pl.edu.agh.mplt.visitors.translator.Translator
import pl.edu.agh.mplt.visitors.Visitor
import pl.edu.agh.mplt.visitors.translator.latex._


class ReferenceIndexer extends Translator[Declaration] {
  def defineEntry(group: String, node: DataDeclaration) = {
    val escapedName = node.name.replace("_", "\\_")
    val glsName = s"\\glssymbol{${node.name}}"
    val indexedName = node.indexing.map(zipWithIndexes(glsName, _)) getOrElse glsName
    val attrs = joinWith(",")(node.attributes.map((new AttributeTranslator(glsName))(_)))

    s"\\newglossaryentry{${node.name}} {\n" +
      s"    symbol={\\ensuremath{${escapedName}}},\n" +
      s"    type=$group,\n" +
      s"    name={\\ensuremath{${indexedName}}},\n" +
      s"    description={\\ensuremath{$attrs} DESCRIBE ME}\n" +
      s"}"
  }

  private def zipWithIndexes(name: String, indexing: Indexing): String = {
    val indices = joinWith(",")(indexing.sexprs.flatMap{case IndexedSet(is, _) => is })

    if(indices != "" && indices != " ") s"${name}_{$indices}"
    else name
  }

  override def apply(node: Declaration): String = node match {
    case a: Assertion             => ""
    case d: ParameterDeclaration  => {
      val definition: String = d.indexing.map(node => node.sexprs.map(s => (new SexprTranslator)(s)).reverse.mkString(", ")).mkString(", ")
      defineEntry("param", d)
    }
    case v: VariableDeclaration   => defineEntry("var", v)
    case s: SetDeclaration        => defineEntry("set", s)
    case o: ObjectiveDeclaration  => ""
    case c: ConstraintDeclaration => ""

    case InvalidDeclaration(msg) => ""

    case dec => throw new Error(s"Unexpected token: $dec")
  }

}
