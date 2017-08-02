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

      def isEdge(c: Int, r: Int): Boolean = {
        if (c == 0 || r == c) true else false
      }

      if (isEdge(c,r)) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      var parenthesis = 0

      def updateParenthesisBalance(char:Char): Unit = {
        if (char == '(') parenthesis +=  1
        if (char == ')') parenthesis -=  1
      }


      def balanceR(charsR:List[Char]): Boolean = {

        updateParenthesisBalance(charsR.head)

        if (charsR.tail.isEmpty) {
          parenthesis == 0
        } else {
          if (parenthesis >=0) balanceR(charsR.tail) else false
        }

      }

      balanceR(chars)

    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeRec(currentMoney: Int, currentCoins: List[Int]): Int = {
        if (currentMoney == money)
          1
        else if (currentMoney > money || currentCoins.isEmpty)
          0
        else
          countChangeRec(currentMoney + currentCoins.head, currentCoins) +
            countChangeRec(currentMoney, currentCoins.tail)
      }

      countChangeRec(0, coins)
    }
  }
