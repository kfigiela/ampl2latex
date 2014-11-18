package pl.edu.agh.mplt

import pl.edu.agh.mplt.visitors.translator.latex.LatexTranslator
import pl.edu.agh.mplt.visitors.translator.mappers.{IndexingFixer, NameFixer, ParenthesisFixer, ParenthesisStripper}

trait TranslatorActions {
   private def removeEveryParenthesis() = new ParenthesisStripper

   private def addNecessaryParenthesis() = new ParenthesisFixer

   private def fixParenthesis = removeEveryParenthesis andThen addNecessaryParenthesis

   private def fixNames = new NameFixer

   private def fixIndexings = new IndexingFixer

   private def translateToLatex = new LatexTranslator

   protected def latexTranslator = fixParenthesis andThen fixNames andThen fixIndexings andThen translateToLatex
}
