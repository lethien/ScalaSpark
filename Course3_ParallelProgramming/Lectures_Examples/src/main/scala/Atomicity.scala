trait Atomicity {
  var uidCount = 0L
  var t1, t2: Thread = null

  def getUniqueIdNonAtomically(): Long = {
    uidCount = uidCount + 1
    uidCount
  }

  val x = new AnyRef
  def getUniqueIdAtomically(): Long = x.synchronized {
    uidCount = uidCount + 1
    uidCount
  }

  def threadNonSync() = {
    val t = new Thread {
      override def run(): Unit = {
        val uids = for(i <- 1 to 10) yield getUniqueIdNonAtomically()
        println(uids)
      }
    }
    t.start()
    t
  }

  def threadSync() = {
    val t = new Thread {
      override def run(): Unit = {
        val uids = for(i <- 1 to 10) yield getUniqueIdAtomically()
        println(uids)
      }
    }
    t.start()
    t
  }

  // Test non sync
  def testNonSync() = {
    uidCount = 0L
    t1 = threadNonSync()
    t2 = threadNonSync()
    t1.join()
    t2.join()
  }

  // Test sync
  def testSync() = {
    uidCount = 0L
    t1 = threadSync()
    t2 = threadSync()
    t1.join()
    t2.join()
  }
}

object TestAtomic extends Atomicity {
  def run() = {
    println("Test Atomicity - Start")
    testNonSync()
    testSync()
    println("Test Atomicity - Done")
  }
}