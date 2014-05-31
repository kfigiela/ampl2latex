package pl.edu.agh.mplt

import pl.edu.agh.mplt.visitors.translator.mappers.{NameFixer, AddNecessaryParenthesis, StripAllParenthesis}
import pl.edu.agh.mplt.visitors.translator.latex.LatexTranslator
import pl.edu.agh.mplt.visitors.Visitor
import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.visitors.translator.Translator


trait Mappers {
  protected def removeEveryParenthesis = new StripAllParenthesis

  protected def addNecessaryParenthesis = new AddNecessaryParenthesis

  protected def fixParenthesis = removeEveryParenthesis andThen addNecessaryParenthesis

  protected def fixNames = new NameFixer

  protected def translateToLatex :Translator[Declaration] = new LatexTranslator


   protected def latexTranslator = fixParenthesis andThen fixNames andThen translateToLatex
}
