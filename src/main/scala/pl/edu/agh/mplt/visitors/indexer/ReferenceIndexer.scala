package pl.edu.agh.mplt.visitors.indexer

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.declaration.InvalidDeclaration
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration
import pl.edu.agh.mplt.parser.declaration.data.{SetDeclaration, VariableDeclaration, ParameterDeclaration, DataDeclaration}
import pl.edu.agh.mplt.parser.declaration.objective.ObjectiveDeclaration
import pl.edu.agh.mplt.parser.phrase.set.{IndexedSet, Indexing}
import pl.edu.agh.mplt.visitors.translator.Translator
import pl.edu.agh.mplt.visitors.translator.latex._


class GlossaryIndexer extends Translator[DataDeclaration] {
  def apply(node: DataDeclaration):String = {
    val (symbol, description) = {
      val s = (node.alias getOrElse "|").split("\\|").map(_.trim)
      if(s.length > 1)
        (s.head, s.tail.mkString("|"))
      else if(s.length == 1 && s.head != "")
        (s.head, "TODO")
      else
        (node.name.replace("_", "\\_"), "TODO")
    }

    val glsName = s"\\glssymbol{${node.name}}"
    val indexedName = node.indexing.map(zipWithIndexes(glsName, _)) getOrElse glsName
    val attrs = joinWith(",")(node.attributes.map((new AttributeTranslator(""))(_)))

    s"\\newglossaryentry{${node.name}} {\n" +
      s"    symbol={\\ensuremath{${symbol}}},\n" +
      s"    name={\\ensuremath{${indexedName} $attrs}},\n" +
      s"    description={$description}\n" +
      s"}"
  }

  private def zipWithIndexes(name: String, indexing: Indexing): String = {
    val indices = joinWith(",")(indexing.sexprs.flatMap{case IndexedSet(is, _) => is })

    if(indices != "" && indices != " ") s"${name}[_{$indices}]"
    else name
  }
}

class ConstraintIndexer extends Translator[ConstraintDeclaration] {
  def apply(node: ConstraintDeclaration): String = {
    node.alias.map(alias => s"\\item \\ref{constraint:${node.name}} $alias") getOrElse ""
  }
}

class ObjectiveIndexer extends Translator[ObjectiveDeclaration] {
  def apply(node: ObjectiveDeclaration): String = {
    node.alias.map(alias => s"\\item \\ref{objective:${node.name}} $alias") getOrElse ""
  }
}


class ReferenceIndexer extends Translator[Declaration] {
  override def apply(node: Declaration): String = node match {
    case a: Assertion             => ""
    case d: DataDeclaration       => (new GlossaryIndexer())(d)
    case o: ObjectiveDeclaration  => (new ObjectiveIndexer())(o)
    case c: ConstraintDeclaration => (new ConstraintIndexer())(c)

    case InvalidDeclaration(msg) => ""

    case dec => throw new Error(s"Unexpected token: $dec")
  }
}
