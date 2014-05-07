package pl.edu.agh.mplt.manipulators.latex

import pl.edu.agh.mplt.parser.declaration.datatype._
import pl.edu.agh.mplt.parser.declaration.datatype.VariableDeclaration
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.declaration.datatype.ParameterDeclaration


trait DataDeclarationTranslator {
  def translateIndexing(i: Indexing): String

  def translateAttribute(attr: Attribute)(name: String): String

  def translateData(data: DatatypeDeclaration): String = data match {
    case ParameterDeclaration(name, alias, indexing, attributes) =>
      s"param \\ ${translate(name, alias, indexing, attributes)}"
    case VariableDeclaration(name, alias, indexing, attributes) =>
      s"var \\ ${translate(name, alias, indexing, attributes)}"
    case SetDeclaration(name, alias, indexing, attributes) =>
      s"set \\ ${translate(name, alias, indexing, attributes)}"
  }

  def translate(name: String, alias: Option[String], indexing: Option[Indexing], attributes: List[Attribute]): String =
    s"$name : ${indexing.fold("")(translateIndexing)} ${translate(attributes)(name)}"

  def translate(attrs: List[Attribute])(name: String): String = {
    attrs match {
      case Nil => ""
      case hd :: tl => (translateAttribute(hd)(name) /: tl)(_ + ",\\ " + translateAttribute(_)(name))
    }
  }

  //TODO
  def translate(attr: Attribute): String = ""
}
