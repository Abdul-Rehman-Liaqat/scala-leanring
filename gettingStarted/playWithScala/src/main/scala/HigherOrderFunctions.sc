object higherOrderFunctions {
  def sumInts(a: Int, b: Int): Int =
    if(a > b) 0 else a + sumInts(a + 1, b)
}