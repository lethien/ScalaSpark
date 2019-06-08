package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parentheses Balance")
    println(balance(":-)".toList))
    println(balance("())(".toList))

    println("Counting Changes")
    println(countChange(2, List(1,2)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c<0 || r<0 || c>r) throw new IllegalArgumentException("Columns can never be bigger than lines")
      else if(c==0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def iter(balance: Int, chars: List[Char]): Boolean = {
        if(chars.isEmpty || balance < 0) balance == 0
        else if(chars.head.equals('(')) iter(balance + 1, chars.tail)
        else if(chars.head.equals(')')) iter(balance - 1, chars.tail)
        else iter(balance, chars.tail)
      }

      iter(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val sortedCoins = coins.sorted

      def findWays(money: Int, possibleCoins: List[Int]): Int = {
        if(money == 0) 1
        else exploreWays(money, possibleCoins.filter(_ <= money))
      }

      def exploreWays(money: Int, possibleCoins: List[Int]): Int = {
        if(possibleCoins.isEmpty) 0
        else findWays(money - possibleCoins.head, possibleCoins) + exploreWays(money, possibleCoins.tail)
      }

      if(money == 0 || sortedCoins.isEmpty) 0
      else findWays(money, sortedCoins)
    }
  }
