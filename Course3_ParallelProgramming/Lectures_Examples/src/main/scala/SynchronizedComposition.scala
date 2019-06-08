trait SyncComp {
  class Account(private var amount: Int = 0) {
    def getAmount() = amount
    def transfer(target: Account, amount: Int) =
      this.synchronized {
        target.synchronized {
          this.amount -= amount
          target.amount += amount
        }
      }
  }

  def startThread(a: Account, b: Account, n: Int) = {
    val t = new Thread {
      override def run(): Unit = {
        println("Start transfering " + n)
        for(i <- 0 until n) {
          a.transfer(b, 1)
        }
        println("From: " + a.getAmount)
        println("To: " + b.getAmount)
      }
    }
    t.start()
    t
  }
}

object SynchronizedComposition extends SyncComp {
  def run() = {
    println("Test Synchronized Composition - Start")
    val a1 = new Account(50000)
    val a2 = new Account(100000)
    val t1 = startThread(a1, a2, 15000)
    val t2 = startThread(a2, a1, 30000)
    t1.join(10000)
    t2.join(10000)
    println("Test Synchronized Composition - Done")
  }
}
