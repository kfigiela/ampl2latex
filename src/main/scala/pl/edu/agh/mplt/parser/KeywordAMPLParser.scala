package pl.edu.agh.mplt.parser

import scala.util.parsing.combinator.JavaTokenParsers
;

trait KeywordAMPLParser extends JavaTokenParsers {
  def nonKeyword: Parser[String] = string ^? {
    case msg if !keywords.contains(msg) => msg
  }

  def keyword: Parser[String] = keywordParser

  private def string = "[a-zA-Z][a-zA-Z_0-9]*".r

  private[this] val keywords: List[String] = List(
    "all", "binary", "by", "check", "complements", "contains", "Current", "default",
    "dimen", "div", "else", "environ", "exists", "forall", "if", "IN", "in", "Infinity",
    "Initial", "INOUT", "integer", "less", "LOCAL", "logical", "minimize", "maximize", "max",
    "min", "option", "OUT", "setof", "shell_exitcode", "solve_exitcode", "solve_message", "solve_result",
    "solve_result_num", "suffix", "sum", "symbolic", "table", "then", "union", "until",
    "while", "within")

  private[this] val keywordParser: Parser[String] = keywords.map(Parser(_)).reduce(_ | _)

}
