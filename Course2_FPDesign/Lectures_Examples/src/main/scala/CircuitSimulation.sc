
trait Simulation {
  type Action = () => Unit

  private var curTime = 0
  def currentTime: Int  = curTime

  def afterDelay(delay: Int)(block : => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }
  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest if first.time <= item.time => first :: insert(rest, item)
    case _ => item :: ag
  }

  private def loop(): Unit = agenda match {
    case first :: rest => {
      agenda = rest
      curTime = first.time
      first.action()
      loop()
    }
    case Nil =>
  }
  def run(): Unit = {
    afterDelay(0) {
      println("Simulation started, time = " + currentTime)
    }
    loop()
  }

  case class Event(time: Int, action: Action)
  private type Agenda = List[Event] // kept sorted by time
  private var agenda: Agenda = List()
}

trait Parameters {
  def InverterDelay = 2
  def AndGateDelay = 3
  def OrGateDelay = 5
}

class Gates extends Simulation with Parameters {
  // wire that connect circuit gates
  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()
    def getSigVal: Boolean = sigVal
    def setSigVal(s: Boolean): Unit = if(s != sigVal) {
      sigVal = s
      for(a <- actions) a()
    }
    def addAction(a: Action) = {
      actions = a :: actions
      a()
    }
  }

  // types of circuit gates
  def inverter(input: Wire, output: Wire) = {
    def invertAction() = {
      val inputSig = input.getSigVal
      afterDelay(InverterDelay) { output setSigVal !inputSig}
    }
    input addAction invertAction
  }
  def orGate(input1: Wire, input2: Wire, output: Wire) = {
    def orAction() = {
      val input1Sig = input1.getSigVal
      val input2Sig = input2.getSigVal
      afterDelay(OrGateDelay) { output setSigVal (input1Sig | input2Sig)}
    }
    input1 addAction orAction
    input2 addAction orAction
  }
  def andGate(input1: Wire, input2: Wire, output: Wire) = {
    def andAction() = {
      val input1Sig = input1.getSigVal
      val input2Sig = input2.getSigVal
      afterDelay(AndGateDelay) { output setSigVal (input1Sig & input2Sig)}
    }
    input1 addAction andAction
    input2 addAction andAction
  }
  def probe(name: String, wire: Wire) = {
    def probeAction() = {
      println(s"$name $currentTime value = ${wire.getSigVal}")
    }
    wire addAction probeAction
  }
}

class Circuits extends Gates {
  // input a, b
  // sum = a | b & - (a & b)
  // carry = a & b
  // Truth table: a + b
  // a  b   sum   carry
  // 0  0   0     0
  // 0  1   1     0
  // 1  0   1     0
  // 1  1   0     1
  def halfAdder(a: Wire, b: Wire, sum: Wire, carry: Wire) = {
    val d = new Wire
    val e = new Wire

    orGate(a, b, d)
    andGate(a, b, carry)
    inverter(carry, e)
    andGate(d, e, sum)
  }

  // built using halfadders
  // calculate a plus b, factor in carry
  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) = {
    val s = new Wire
    val c1 = new Wire
    val c2 = new Wire

    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }
}

object sim extends Circuits {
  val input1, input2, sum, carry = new Wire

  halfAdder(input1, input2, sum, carry)
  probe("sum", sum)
  probe("carry", carry)

  input1 setSigVal true
}

sim.run()