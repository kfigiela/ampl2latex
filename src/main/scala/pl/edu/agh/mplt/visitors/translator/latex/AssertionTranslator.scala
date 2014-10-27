package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.visitors.translator.Translator


class AssertionTranslator extends Translator[Assertion] {
   override def apply(node: Assertion): String = ""
}
