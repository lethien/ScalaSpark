
object ObserverPattern {
  trait Subscriber {
    def handler(pub: Publisher)
  }

  trait Publisher {
    private var subscribers: Set[Subscriber] = Set()

    def subscribe(subscriber: Subscriber) = subscribers += subscriber

    def unsubscribe(subscriber: Subscriber) = subscribers -= subscriber

    def publish() = subscribers.foreach(_.handler(this))
  }

  class BankAccount extends Publisher {
    private var balance = 0 // mutable variable
    def currentBalance = balance

    def deposit(amount: Int) = {
      if(amount > 0) balance = balance + amount
      publish()
    }
    def withdraw(amount: Int) = {
      if(0 < amount && amount <= balance) {
        balance = balance - amount
        publish()
      } else throw new Error("insufficient funds")
    }
  }

  class Consolidator(observed: List[BankAccount]) extends Subscriber {
    observed.foreach(_.subscribe(this))

    private var total: Int = _
    compute()

    private def compute() = total = observed.map(_.currentBalance).sum

    def handler(pub: Publisher) = compute()

    def totalBalance = total
  }
}


val a = new ObserverPattern.BankAccount
val b = new ObserverPattern.BankAccount
val c = new ObserverPattern.Consolidator(List(a, b))

println(c.totalBalance)
a deposit 20
println(c.totalBalance)