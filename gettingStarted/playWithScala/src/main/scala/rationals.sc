object rationals{
  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)
  x.add(y).mul(z)
}

class Rational(x: Int, y: Int){
  require(y != 0, "denominator must be nonZero")
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b,  a % b)
  private val g = gcd(x,y)
  val numer = x/g
  val denom = y/g
  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
  def mul(that: Rational) =
    new Rational(
      numer * that.numer,
      denom * that.denom)
  def neg : Rational = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)
  def less(that: Rational) = numer * that.denom < that.numer * denom
  def max(that: Rational) = if(that.less(that)) that else this
  override def toString = numer + "/" + denom
}