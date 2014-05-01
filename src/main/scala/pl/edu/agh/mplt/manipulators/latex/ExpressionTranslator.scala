package pl.edu.agh.mplt.manipulators.latex

import pl.edu.agh.mplt.manipulators.Manipulator
import pl.edu.agh.mplt.parser.formula.expression._
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.reference.SimpleReference

import language.implicitConversions
trait ExpressionTranslator {

  import Bin._
  import ExpressionReduction._

  implicit def expressionPriority(expr: Expression): Int = expr match {
    case +(_, _) | -(_, _) | less(_, _) => 1
    case *(_, _) | /(_, _) | div(_, _) | mod(_, _) => 2
    case ^(_, _) => 3
    case _ => 10
  }

  def translateIndexing(indexing: Indexing): String


  def translateExpression(expr: Expression): String = expr match {
    case Number(n) => n
    case arith: ArithmeticOperation => translateArithmeticExpression(arith)(expressionPriority(arith))
    case ExpressionIf(lexpr, t, f) => s"if $lexpr then $t else $f"
    case fun@FunctionCall(_, _) => translateFunction(fun)
    case PiecewiseLinearTerm(_, _, _) => "plt"
    case ParenthesizedExpression(expr) => s"(${translateExpression (expr)})"
    case SimpleReference(x) => x
  }

  def joinWithDelimeter(left: Expression, right: Expression, delimeter: String)
                       (implicit pr: Int): String = {
    def withParens(arith: Expression) = "{" + translateExpression(arith) + "}"
    val l = if (expressionPriority(left) < pr) withParens(left) else translateExpression(left)
    val r = if (expressionPriority(right) < pr) withParens(right) else translateExpression(right)

    l + delimeter + r
  }

  def translateArithmeticExpression(expr: ArithmeticOperation)(implicit pr: Int): String = expr match {
    case +(left, right) => joinWithDelimeter(left, right, "+")(pr)
    case -(left, right) => joinWithDelimeter(left, right, "-")(pr)
    case less(left, right) => joinWithDelimeter(left, right, "less")(pr)
    case *(left, right) => joinWithDelimeter(left, right, "\\cdot ")(pr)
    case ^(left, right) => joinWithDelimeter(left, right, "^")(pr)
    case div(left, right) => joinWithDelimeter(left, right, "\\div ")(pr)
    case mod(left, right) => joinWithDelimeter(left, right, "\\mod ")(pr)
    case /(left, right) => s"\\frac{${translateExpression(left)}}{${translateExpression(right)}}"


    case Unary.-(expr) => s"- ${translateExpression(expr)}"

    case Sum(indexing: Indexing, expr: Expression) => s"$indexing \\sum {${translateExpression(expr)}}"
    case Prod(indexing: Indexing, expr: Expression) => s"$indexing \\prod {${translateExpression(expr)}}"
    case Max(indexing: Indexing, expr: Expression) => s"$indexing max(${translateExpression(expr)})"
    case Min(indexing: Indexing, expr: Expression) => s"$indexing min(${translateExpression(expr)})"
  }

  def translateFunction(function: FunctionCall): String = {
    val FunctionCall(name, args) = function

    def translateFirstArg: String = translateExpression(args(0))
    def translateSecondArg: String = translateExpression(args(0))
    def translateArgs: String = (translateExpression(args(0)) /: args.drop(1))(_ + ", " + translateExpression(_))

    name match {
      case "abs" => s"|$translateFirstArg|"
      case "atan2" => s"\\arctan {\\frac{$translateFirstArg}{$translateSecondArg}}"
      case "ceil" => s"\\lceil {$translateFirstArg} \\rceil"
      case "floor" => s"\\lfloor {$translateFirstArg} \\rfloor"
      case "exp" => s"e^{$translateFirstArg}"
      case "log" => s"\\ln {$translateFirstArg}"
      case "log10" => s"\\log {$translateFirstArg}"
      case "max" => s"max ($translateArgs)"
      case "min" => s"min ( $translateArgs)"
      case "sqrt" => s"\\sqrt {$translateFirstArg}"
      case _ => s"$name ($translateArgs)"
    }
  }


}
