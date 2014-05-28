package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.Declaration


class AssertionTranslator extends Translator[Assertion] {
  override def apply(node: Assertion): String = ""
}
