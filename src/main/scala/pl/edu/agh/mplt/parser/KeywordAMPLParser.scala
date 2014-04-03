package pl.edu.agh.mplt.parser

import scala.util.parsing.combinator.JavaTokenParsers
;

trait KeywordAMPLParser extends JavaTokenParsers {
  def nonKeyword: Parser[String] = not(keywords) ~> string

  def keyword: Parser[String] = keywords

  private def string = "[a-zA-Z]\\w*".r

  private[this] val keywords =
    List[Parser[String]]("all", "binary", "by", "check", "complements", "contains", "Current", "default",
      "dimen", "div", "else", "environ", "exists", "forall", "if", "IN", "in", "Infinity",
      "Initial", "INOUT", "integer", "less", "LOCAL", "logical", "minimize", "maximize", "max",
      "min", " option", "OUT", "setof", "shell_exitcode", "solve_exitcode", "solve_message", "solve_result",
      "solve_result_num", "suffix", "sum", "symbolic", "table", "then", "union", "until",
      "while", "within").reduce(_ | _)
}
