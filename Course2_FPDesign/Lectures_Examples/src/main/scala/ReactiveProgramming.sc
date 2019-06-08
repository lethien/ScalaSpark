
object ReactiveProgramming {
  class StackableVariable[T](init: T) {
    private var values: List[T] = List(init)
    def value: T = values.head
    def withValue[R](newValue: T)(op: => R): R = {
      values = newValue :: values
      try op finally values = values.tail
    }
  }

  class Signal[T](expr: => T) {
    import Signal._
    private var myExpr: () => T = _
    private var myValue: T = _
    private var observers: Set[Signal[_]] = Set()
    update(expr)

    protected def update(expr: => T): Unit = {
      myExpr = () => expr
      computeValue()
    }
    protected def computeValue(): Unit = {
      val newValue = caller.withValue(this)(myExpr())
      if(myValue != newValue) {
        myValue = newValue
        val obs = observers
        observers = Set()
        obs.foreach(_.computeValue())
      }
    }

    def apply(): T = {
      observers += caller.value
      assert(!caller.value.observers.contains(this), "cyclic signal definition")
      myValue
    }
  }
  object NoSignal extends Signal[Nothing](???) {
    override def computeValue() = ()
  }
  object Signal {
    private val caller = new StackableVariable[Signal[_]](NoSignal)
    def apply[T](expr: => T) = new Signal(expr)
  }

  class Var[T](expr: => T) extends Signal[T](expr) {
    override def update(expr: => T) = super.update(expr)
  }
  object Var {
    def apply[T](expr: => T) = new Var(expr)
  }
}

import ReactiveProgramming._

class BankAccount {
  val balance = Var(0)

  def deposit(amount: Int) = {
    if(amount > 0) {
      val b = balance()
      balance() = b + amount
    }
  }
  def withdraw(amount: Int) = {
    if(0 < amount && amount <= balance()) {
      val b = balance()
      balance() = b - amount
    } else throw new Error("insufficient funds")
  }
}

def consolidated(accts: List[BankAccount]): Signal[Int] = Signal(accts.map(_.balance()).sum)
val a = new BankAccount
val b = new BankAccount
val c = consolidated(List(a, b))

println(c())
a deposit 20
b deposit 50
println(c())


val xchange = Signal(246.00)
val inDollar = Signal(c() * xchange())
println(inDollar())
b withdraw 20
println(inDollar())