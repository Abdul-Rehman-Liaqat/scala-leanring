object week {
  val fruit = List("apples", "oranges", "pears")
  val nums = List(1,2,3)
  val diag3 = List(List(1,0,0), List(0,1,0), List(0,0,1))
  val empty = List()

  def removeAt(n: Int, xs: List[Int])= (xs take n) ::: (xs drop n+1)
  def flatten(xs: List[Any]): List[Any] = xs match {
      case Nil => Nil
      case (h: List[Any]) :: tail => flatten(h) ::: flatten(tail)
      case  (h: Any) :: tail => h :: flatten(tail)

  }

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs : List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y:: ys1) =>
            if(x < y) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
      }
        val (fst,snd) = xs splitAt n
        merge(msort(fst),msort(snd))
    }
  }


  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil =>
    case y :: ys => (x : Int) => x*x :: squareList(ys)
  }

  def squareListwithMap(xs: List[Int]): List[Int] =
    xs map (x : Int) => x*x
}
