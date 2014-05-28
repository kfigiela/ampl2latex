package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.declaration.data.DataDeclaration


class DataTranslator extends Translator[DataDeclaration] {

  override def apply(node: DataDeclaration): String = {
    val name = node.name
    val indexing: String = node.indexing.map(i => (new IndexingTranslator)(i)) getOrElse ""
    val attrs = node.attributes.map((new AttributeTranslator)(_))

    s"$name: $indexing $attrs"
  }
}
