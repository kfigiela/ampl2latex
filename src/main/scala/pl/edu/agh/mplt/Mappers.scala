package pl.edu.agh.mplt

import pl.edu.agh.mplt.visitors.latex.mappers.{NameFixer, AddNecessaryParenthesis, StripAllParenthesis}
import pl.edu.agh.mplt.visitors.latex.tmp.LatexTranslator


trait Mappers {
  protected def removeEveryParenthesis = new StripAllParenthesis

  protected def addNecessaryParenthesis = new AddNecessaryParenthesis

  protected def fixParenthesis = removeEveryParenthesis andThen addNecessaryParenthesis

  protected def fixNames = new NameFixer

  protected def translateToLatex = new LatexTranslator
}
