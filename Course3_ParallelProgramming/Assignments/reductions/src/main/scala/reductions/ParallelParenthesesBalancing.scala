package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def checkBalance(idx: Int, balance: Int): Boolean = {
      if(balance < 0) false
      else {
        if(idx >= chars.length) balance == 0
        else checkBalance(idx + 1, balance + balanceChange(chars(idx)))
      }
    }

    def balanceChange(char: Char): Int = if(char.equals('(')) 1 else if(char.equals(')')) -1 else 0

    checkBalance(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, openRedundant: Int, closeRedundant: Int): (Int, Int) = {
      if(idx >= until) (openRedundant, closeRedundant)
      else {
        val currentChar = chars(idx)
        if(currentChar.equals('(')) traverse(idx + 1, until, openRedundant + 1, closeRedundant)
        else if(currentChar.equals(')')) {
          if(openRedundant > 0) traverse(idx + 1, until, openRedundant - 1, closeRedundant)
          else traverse(idx + 1, until, openRedundant, closeRedundant + 1)
        }
        else traverse(idx + 1, until, openRedundant, closeRedundant)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val middle = (until + from) / 2
        val (l, r) = parallel(reduce(from, middle), reduce(middle, until))

        val openClosedPairs = math.min(l._1, r._2)
        (l._1 + r._1 - openClosedPairs, l._2 + r._2 - openClosedPairs)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
