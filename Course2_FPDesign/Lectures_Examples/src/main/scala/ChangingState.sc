// Stateful object
class BankAccount {
  private var balance = 0 // mutable variable
  def deposit(amount: Int) = {
    if(amount > 0) balance = balance + amount
  }
  def withdraw(amount: Int) = {
    if(0 < amount && amount <= balance) {
      balance = balance - amount
      balance
    } else throw new Error("insufficient funds")
  }
}

val acc = new BankAccount
acc deposit 50
acc withdraw 30
acc withdraw 25