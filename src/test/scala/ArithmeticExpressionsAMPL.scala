import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.expression.arithmetic.{Unary, Bin, ArithmeticParser}
import pl.edu.agh.mplt.parser.expression.{Number, ExpressionParser}

class ArithmeticExpressionsAMPL extends FlatSpec with Matchers {

  implicit def intToString(i: Int): String = i.toString

  implicit def intToNumber(i: Int): Number = Number(i)

  val parser = new ExpressionParser with ArithmeticParser

  def expr = parser.expr

  def parse(input: String) = parser.parse(expr, input).get

  "Arithmetic parser" should "parse addition" in {
    parse("1 + 2") should be(Bin.+(Number(1), Number(2)))
  }

  it should "parse subtraction" in {
    parse("1 - 2") should be(Bin.-(Number(1), Number(2)))
  }

  it should "parse less operator" in {
    parse("1 less 2") should be(Bin.less(Number(1), Number(2)))
  }

  it should "parse multiplication" in {
    parse("1 * 2") should be(Bin.*(Number(1), Number(2)))
  }

  it should "parse division" in {
    parse("1 / 2") should be(Bin./(Number(1), Number(2)))
  }

  it should "parse modulo operator" in {
    parse("1 mod 2") should be(Bin.mod(Number(1), Number(2)))
  }

  it should "parse integer division" in {
    parse("1 div 2") should be(Bin.div(Number(1), Number(2)))
  }

  it should "parse power operator" in {
    parse("1 ^ 2") should be(Bin.^(Number(1), Number(2)))
  }

  it should """parse "**" operator same as"^" """ in {
    parse("1 ** 2") should be(parse("1 ^ 2"))
  }

  it should "parse unary +" in {
    parse("+1") should be(Number(1))
  }

  it should "parse unary -" in {
    parse("-1") should be(Unary.-(Number(1)))
  }

  it should "parse parenthesis" in {
    parse("(1 + 2) * 3") should be(Bin.*(Bin.+(Number(1), Number(2)), Number(3)))
  }

  ////////////////////////////////////////////

  it should "maintain left associativity of addition" in {
    parse("1 + 2 + 3") should be(Bin.+(Bin.+(Number(1), Number(2)), Number(3)))
  }

  it should "maintain left associativity of subtraction" in {
    parse("1 - 2 - 3") should be(Bin.-(Bin.-(Number(1), Number(2)), Number(3)))
  }

  it should "maintain left associativity of less operator" in {
    parse("1 less 2 less 3") should be(Bin.less(Bin.less(Number(1), Number(2)), Number(3)))
  }

  it should "maintain left associativity of multiplication" in {
    parse("1 * 2 * 3") should be(Bin.*(Bin.*(Number(1), Number(2)), Number(3)))
  }

  it should "maintain left associativity of division" in {
    parse("1 / 2 / 3") should be(Bin./(Bin./(Number(1), Number(2)), Number(3)))
  }

  it should "maintain left associativity of modulo" in {
    parse("1 mod 2 mod 3") should be(Bin.mod(Bin.mod(Number(1), Number(2)), Number(3)))
  }

  it should "maintain left associativity of integer division" in {
    parse("1 div 2 div 3") should be(Bin.div(Bin.div(Number(1), Number(2)), Number(3)))
  }

  it should "maintain right associativity of exponentiation" in {
    parse("1 ^ 2 ^ 3") should be(Bin.^(Number(1), Bin.^(Number(2), Number(3))))
  }

  it should "maintain left associativity of unary addition" in {
    parse("+1 - 2") should be(Bin.-(Number(1), Number(2)))
  }

  it should "maintain left associativity of unary subtraction" in {
    parse("-1 + 2") should be(Bin.+(Unary.-(Number(1)), Number(2)))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "multiplication" should "precede addition" in {
    parse("1 + 2 * 3") should be(Bin.+(1, Bin.*(2, 3)))
  }

  it should "precede subtraction" in {
    parse("1 - 2 * 3") should be(Bin.-(1, Bin.*(2, 3)))
  }

  it should "precede less" in {
    parse("1 less 2 * 3") should be(Bin.less(1, Bin.*(2, 3)))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "division" should "precede addition" in {
    parse("5 + 3 / 2") should be(Bin.+(Number(5), Bin./(Number(3), Number(2))))
  }

  it should "precede subtraction" in {
    parse("5 - 3 / 2") should be(Bin.-(Number(5), Bin./(Number(3), Number(2))))
  }

  it should "precede less" in {
    parse("5 less 3 / 2") should be(Bin.less(Number(5), Bin./(Number(3), Number(2))))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "integer division" should "precede addition" in {
    parse("5 + 3 div 2") should be(Bin.+(Number(5), Bin.div(Number(3), Number(2))))
  }

  it should "precede subtraction" in {
    parse("5 - 3 div 2") should be(Bin.-(Number(5), Bin.div(Number(3), Number(2))))
  }

  it should "precede less" in {
    parse("5 less 3 div 2") should be(Bin.less(Number(5), Bin.div(Number(3), Number(2))))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "modulo" should "precede addition" in {
    parse("5 + 3 mod 2") should be(Bin.+(Number(5), Bin.mod(Number(3), Number(2))))
  }

  it should "precede subtraction" in {
    parse("5 - 3 mod 2") should be(Bin.-(Number(5), Bin.mod(Number(3), Number(2))))
  }

  it should "precede less" in {
    parse("5 less 3 mod 2") should be(Bin.less(Number(5), Bin.mod(Number(3), Number(2))))
  }
  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "unary minus" should "precede division" in {
    parse("-1 / 3") should be(Bin./(Unary.-(Number(1)), Number(3)))
  }

  it should "precede integer division" in {
    parse("-1 div 3") should be(Bin.div(Unary.-(Number(1)), Number(3)))
  }

  it should "precede modulo" in {
    parse("-1 mod 3") should be(Bin.mod(Unary.-(Number(1)), Number(3)))
  }

  it should "precede multiplication" in {
    parse("-1 * 3") should be(Bin.*(Unary.-(Number(1)), Number(3)))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "exponentiation" should "precede unary plus" in {
    parse("+1 ^ 3") should be(Bin.^(Number(1), Number(3)))
  }

  it should "precede unary minus" in {
    parse("-1 ^ 3") should be(Unary.-(Bin.^(Number(1), Number(3))))
  }

}
