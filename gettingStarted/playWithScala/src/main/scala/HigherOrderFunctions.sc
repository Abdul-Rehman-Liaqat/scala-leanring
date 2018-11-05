object higherOrderFunctions {

  def cube(x: Int): Int = x * x * x

  def fact(x: Int): Int =
    if(x == 0) 1 else x * fact(x-1)

  def id(x: Int): Int = x

  def sumCubes(a: Int, b: Int): Int =
    if( a > b) 0 else cube(a) + sumCubes(a + 1, b)

  def sumFactorials(a: Int, b: Int): Int =
    if( a > b) 0 else fact(a) + sumFactorials(a + 1, b)

  def sumInts(a: Int, b: Int): Int =
    if(a > b) 0 else a + sumInts(a + 1, b)

  // Using higher order functions

  def sum(f: Int => Int, a: Int, b: Int): Int=
    if(a > b) 0
    else f(a) + sum(f, a + 1, b)


}