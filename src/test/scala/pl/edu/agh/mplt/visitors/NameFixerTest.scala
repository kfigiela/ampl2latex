package pl.edu.agh.mplt.visitors

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.IntercodeImplicits
import pl.edu.agh.mplt.visitors.translator.mappers.NameFixer


class NameFixerTest extends FlatSpec with Matchers with VisitorImplicits with IntercodeImplicits {
  def mapper: NodeMapper = new NameFixer

  "Name fixer" should "fix '_' in variable declarations " in {
    map("var a_b;").toName should be("a\\_b")
  }

  it should "fix '_' in parameter declarations " in {
    map("param a_b;").toName should be("a\\_b")
  }

  it should "fix '_' in set declarations " in {
    map("set a_b;").toName should be("a\\_b")
  }

  it should "fix '_' in simple reference" in {
    map("var a  = a_b;").toExpr.toName should be("a\\_b")
}

  it should "fix '_' in indexed reference " in {
    map("var a_b = a_b[i];").toExpr.toName should be("a\\_b")
  }
}
