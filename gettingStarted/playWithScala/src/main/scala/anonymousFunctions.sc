object anonymousFunctions {
  (x: Int) => x * x * x
  (x: Int, y: Int) => x + y

  def sum(f: Int => Int, a: Int, b: Int ): Int = {
    def loop(a: Int, acc: Int): Int = {
      if(a > b) acc
      else loop(a+1, acc)
    }
    loop(a,0)
  }
}