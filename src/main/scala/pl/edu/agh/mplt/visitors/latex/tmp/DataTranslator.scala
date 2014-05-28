package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.declaration.data.DataDeclaration
import pl.edu.agh.mplt.visitors.latex.mappers.NameFixer


class DataTranslator extends Translator[DataDeclaration]{
  override def apply(node: DataDeclaration): String = {
    val name = node.name
    val indexing = (new IndexingTranslator)(node.indexing)
    val attrs = node.attributes.map((new AttributeTranslator)(_))

    s"$name: $indexing $attrs"
  }
}
