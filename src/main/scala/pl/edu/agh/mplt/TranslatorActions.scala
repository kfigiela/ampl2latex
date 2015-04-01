package pl.edu.agh.mplt

import pl.edu.agh.mplt.visitors.indexer.ReferenceIndexer
import pl.edu.agh.mplt.visitors.translator.latex.LatexTranslator
import pl.edu.agh.mplt.visitors.translator.mappers._

trait TranslatorActions {
   private def removeEveryParenthesis() = new ParenthesisStripper

   private def addNecessaryParenthesis() = new ParenthesisFixer

   private def fixParenthesis = removeEveryParenthesis andThen addNecessaryParenthesis

   private def fixNames = new NameFixer

   private def fixNamesGls = new GlsFixer

   private def fixIndexings = new IndexingFixer

   private def translateToLatex = new LatexTranslator

   private def indexRefs = new ReferenceIndexer

   private def preprocessTree =  fixParenthesis andThen fixIndexings

   protected def latexTranslator =  preprocessTree andThen fixNamesGls andThen translateToLatex

   protected def referenceIndexer =  preprocessTree andThen indexRefs
}
