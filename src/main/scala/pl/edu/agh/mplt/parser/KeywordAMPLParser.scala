package pl.edu.agh.mplt.parser

import scala.util.parsing.combinator.JavaTokenParsers
;

trait KeywordAMPLParser extends JavaTokenParsers {
  def nonKeyword: Parser[String] = string ^? {
    case msg if !keywords.contains(msg) => msg
  }

  private def string = "[a-zA-Z][a-zA-Z_0-9]*".r

  private[this] val ops = List[String]("<", "<=", "=", "==", "!=", "<>", ">", ">=", "+", "-", "*", "/", "^", "**", "and", "&&",
    "or", "||", "not", "!", "product", "prod")

  private[this] val keywords: List[String] = ops ++ List("#",
    "all", "binary", "by", "check", "coeff", "complements", "contains", "cover", "Current", "default",
    "dimen", "div", "else", "environ", "exists", "forall", "if", "IN", "in", "Infinity",
    "Initial", "INOUT", "integer", "less", "LOCAL", "logical", "minimize", "maximize", "max",
    "min", "obj", "option", "OUT", "setof", "shell_exitcode", "solve_exitcode", "solve_message", "solve_result",
    "solve_result_num", "suffix", "sum", "symbolic", "table", "then", "union", "until",
    "while", "within")

  def keyword: Parser[String] = keywords.map(Parser(_)).reduce(_ | _)

  def op: Parser[String] = ops.map(Parser(_)).reduce(_ | _)

}
