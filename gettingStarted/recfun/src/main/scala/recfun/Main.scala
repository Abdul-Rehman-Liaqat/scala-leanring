package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */

    def pascal(c: Int, r: Int): Int = {
      if(c == 0 || r == 0 || r == 1 || c == r) 1
      else if(c-1 >= 0)
        pascal(c-1,r-1)+pascal(c,r-1)
      else
        pascal(c,r-1)
    }



  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def countOpenClose(countOpen: Int, res: List[Char]): Int = {
        if (res.isEmpty) countOpen
        else if (res.head == '(') countOpenClose(countOpen + 1, res.tail)
        else if (res.head == ')' && countOpen > 0) countOpenClose(countOpen - 1, res.tail)
        else if (res.head == ')' && countOpen <= 0) -1
        else countOpenClose(countOpen, res.tail)
      }
      val countOpen = countOpenClose(0,chars)
      countOpen == 0
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(m: Int, c: List[Int]): Int = {
      if (c.isEmpty) 0
      else if (m - c.head == 0) 1
      else if (m - c.head < 0) 0
      else countChange(m - c.head, c) + countChange(m, c.tail)
    }
    count(money, coins.sorted)
  }}
