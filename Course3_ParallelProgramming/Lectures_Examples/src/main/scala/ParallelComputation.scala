trait Task[A] {
  def join: A
}

def task[A](c: => A): Task[A]

class ParallelComputation {
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val ta = taskA
    val tb = task {taskB}
    (ta, tb.join)
  }
}
