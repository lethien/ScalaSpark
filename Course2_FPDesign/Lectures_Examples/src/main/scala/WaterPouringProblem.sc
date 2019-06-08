// Given n glasses with pre-defined capacity (in liter)
// Given 3 possible moves: Fill a glass, empty a glass, pour all from a glass to another
// Find moves sequence that by the end, have a glass with desired amount

class Pouring(capacity: Vector[Int]) {
  // States of glasses
  type State = Vector[Int]
  val initialState = capacity map (x => 0)

  // Moves
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State) = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State) = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  // Paths
  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move change endState)

    override def toString = (history.reverse mkString " ") + "-->" + endState
  }
  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if(paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        nextMove <- moves map path.extend
        if !(explored contains nextMove.endState)
      } yield nextMove
      paths #:: from(more, explored ++ (more map (_.endState))) // concat stream
    }

  val pathSets = from(Set(initialPath), Set(initialState)) // Lazy infinite stream of sets path

  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}

val problem = new Pouring(Vector(4, 7, 19))

println(problem.moves)

println(problem.solutions(17))