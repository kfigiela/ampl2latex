package pl.edu.agh.mplt.manipulators.latex

import pl.edu.agh.mplt.parser.declaration.datatype._
import pl.edu.agh.mplt.parser.declaration.datatype.VariableDeclaration
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.declaration.datatype.ParameterDeclaration


trait DataDeclarationTranslator {
  def translateIndexing(i: Indexing): String

  def translateData(data: DatatypeDeclaration): String = data match {
    case ParameterDeclaration(name, alias, indexing, attributes) =>
      s"param $name ${translate(alias, indexing, attributes)}"
    case VariableDeclaration(name, alias, indexing, attributes) =>
      s"var $name ${translate(alias, indexing, attributes)}"
    case SetDeclaration(name, alias, indexing, attributes) =>
      s"set $name ${translate(alias, indexing, attributes)}"
  }

  def translate(alias: Option[String], indexing: Option[Indexing], attributes: List[Attribute]): String =
    s"${if (indexing.isEmpty) "" else indexing.map(translateIndexing)} ${translate(attributes)}"

  def translate(attrs: List[Attribute]): String = {
    attrs match {
      case Nil => ""
      case hd :: tl => (translate(hd) /: tl)(_ + ", " + translate(_))
    }
  }

  //TODO
  def translate(attr: Attribute): String = ""
}
