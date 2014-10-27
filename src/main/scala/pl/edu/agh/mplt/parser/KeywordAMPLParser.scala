package pl.edu.agh.mplt.parser

import scala.util.parsing.combinator.JavaTokenParsers
;

trait KeywordAMPLParser extends JavaTokenParsers {
   def nonKeyword: Parser[String] = string ^? {
      case str if !keywords.contains(str) => str
   }

   def nonAttributeKeyword: Parser[String] = nonKeyword ^? {
      case str if !attributeKeyword.contains(str) => str
   }

   def refName: Parser[String] = nonKeyword ^? {
      case str if !exprReducuctionOps.contains(str) => str
   }

   def functionName: Parser[String] = nonKeyword

   private def string = "[a-zA-Z][a-zA-Z_0-9]*".r

   private[this] val attributeKeyword: List[String] = List("binary", "integer", "symbolic", "in", "dimen", "within",
      "default", "coeff", "cover", "obj")

   private[this] val exprReducuctionOps: List[String] = List("max", "min", "prod", "sum")

   private[this] val keywords: List[String] = List("#",
      "all", "binary", "by", "check", "coeff", "complements", "contains", "cover", "Current", "default",
      "dimen", "div", "else", "environ", "exists", "forall", "if", "IN", "in", "Infinity",
      "Initial", "INOUT", "integer", "less", "LOCAL", "logical", "minimize", "maximize",
      "option", "OUT", "setof", "shell_exitcode", "solve_exitcode", "solve_message", "solve_result",
      "solve_result_num", "suffix", "symbolic", "table", "then", "union", "until",
      "while", "within")

   def keyword: Parser[String] = keywords.map(Parser(_)).reduce(_ | _)

   def exprReductionOp: Parser[String] = exprReducuctionOps.map(Parser(_)).reduce(_ | _)

}
