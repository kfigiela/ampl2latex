package pl.edu.agh.mplt.parser

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.matching.Regex

trait CommentAMPLParser extends JavaTokenParsers {

  private def comment: Regex = """(\s|#.*)+""".r
//  whiteSpace
  protected override val whiteSpace = comment

}
