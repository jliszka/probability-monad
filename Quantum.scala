case class W[A, B](state: (A, B)*)(implicit num: Numeric[B], num2: Numeric2[B]) {
  private val rand = new scala.util.Random()
  import num._
  private val _m = state.toMap
  def apply(a: A): B = _m.getOrElse(a, num.zero)

  def map[A1](f: A => A1): W[A1, B] = {
    W(state.map{ case (a, b) => (f(a), b) }: _*).collect
  }
  def mapV[B1: Numeric: Numeric2](f: B => B1): W[A, B1] = {
    W(state.map{ case (a, b) => (a, f(b)) }: _*)
  }

  def flatMap[A1](f: A => W[A1, B]): W[A1, B] = {
    W(state.flatMap{ case (a, b) => f(a).mapV(_ * b).state }: _*).collect
  }
  def >>=[A1](f: A => W[A1, B]): W[A1, B] = this.flatMap(f)

  // Collect like terms and sum their coefficients
  def collect: W[A, B] = {
    W(state.groupBy(_._1).toList.map{ case (a1, abs) => (a1, abs.map(_._2).sum) }: _*)
  }

  def scale(x: Double) = {
    W(state.map{ case (a, b) => (a, num2.scale(b, x)) }: _*)
  }
  def unary_- = this.scale(-1.0)

  def filter(f: A => Boolean)(implicit num: Fractional[B]): W[A, B] = {
    W(state.filter{ case (a, b) => f(a) }: _*).normalize
  }

  // Make sure the sum of the squares of the coefficients is 1
  def normalize(implicit frac: Fractional[B]) = {
    val total = math.sqrt(state.map{ case (a, b) => num2.norm(b) }.sum)
    this.scale(1 / total)
  }

  // Measure a quantum state (or a part of one). Returns the outcome of the measurement and the new state.
  def measure[A1](w: A => A1 = identity[A] _)(implicit frac: Fractional[B]): (A1, W[A, B]) = {
    val r = rand.nextDouble()
    def find(r: Double, s: List[(A, Double)]): A = s match {
      case (l, p) :: Nil => l
      case (l, p) :: rest if r < p => l
      case (l, p) :: rest => find(r - p, rest)
      case Nil => throw new Exception("empty state")
    }
    val squaredAmplitudes = this.state.toList.map{ case (a, b) => (a, num2.norm(b)) }
    val outcome = w(find(r, squaredAmplitudes))
    val newState = this.filter(s => w(s) == outcome)
    (outcome, newState)
  }

  def inner(other: W[A, B]) {
    val m = other.state.toMap
    W((for {
      (l, v1) <- state
      v2 <- m.get(l)
    } yield (l, num2.conj(v1) * v2)) :_*)
  }

  override def toString = {
    state
      .filter{ case (a, b) => num2.norm(b) > 0.00001 }
      .sortBy{ case (a, b) => a.toString }
      .map{ case (a, b) => b.toString + a.toString }
      .mkString(" + ")
  }
}

class WIsNumeric[A, B](implicit num: Numeric[B], num2: Numeric2[B]) extends Numeric[W[A, B]] {
  private def ??? = throw new Exception
  override def compare(x: W[A, B], y: W[A, B]) = if (x.state == y.state) 0 else 1
  override def fromInt(x: Int) = ???
  override def plus(x: W[A, B], y: W[A, B]) = W(x.state ++ y.state :_*).collect
  override def minus(x: W[A, B], y: W[A, B]) = plus(x, negate(y))
  override def negate(x: W[A, B]) = -x
  override def times(x: W[A, B], y: W[A, B]) = ???
  override def toDouble(x: W[A, B]) = ???
  override def toFloat(x: W[A, B]) = ???
  override def toInt(x: W[A, B]) = ???
  override def toLong(x: W[A, B]) = ???
}

object W {
  type Q[A] = W[A, Complex]
  implicit def wIsNumeric[A, B: Numeric: Numeric2]: WIsNumeric[A, B] = new WIsNumeric[A, B]
  val rhalf: Complex = math.sqrt(0.5)
  val rquarter: Complex = math.sqrt(0.75)
  def pure[A](a: A): Q[A] = new W(a -> Complex.one)
}

abstract class Basis[+B <: Basis[B]](val label: String, vs: B*) {
  def vectors: List[B] = vs.toList
  override def toString = "|"+label+">"
}

object Basis {
  import W._

  // Standard basis { |0>, |1> }
  sealed abstract class Std(label: String) extends Basis[Std](label, S0, S1)
  case object S0 extends Std("0")
  case object S1 extends Std("1")

  val s0: Q[Std] = pure(S0)
  val s1: Q[Std] = pure(S1)
  val plus: Q[Std] = W(S0 -> rhalf, S1 -> rhalf)
  val minus: Q[Std] = W(S0 -> rhalf, S1 -> -rhalf)

  // Sign basis { |+>, |-> }
  sealed abstract class Sign(label: String) extends Basis[Sign](label, S_+, S_-)
  case object S_+ extends Sign("+")
  case object S_- extends Sign("-")

  val s_+ = pure(S_+)
  val s_- = pure(S_-)

  // Tensor product of two bases, e.g., T[Std, Std] = { |00>, |01>, |10>, |11> }
  case class T[+B1 <: Basis[B1], +B2 <: Basis[B2]](_1: B1, _2: B2) extends Basis[T[B1, B2]](_1.label + _2.label) {
    override def vectors = {
      for {
	b1 <- _1.vectors
	b2 <- _2.vectors
      } yield T(b1, b2)
    }
  }
}


object Gate {
  import W.{pure, rhalf, Q}
  import Basis._
  
  // A unitary transformation (a quantum gate)
  type U[B <: Basis[B]] = B => Q[B]

  // Identity gate
  def I[B <: Basis[B]](b: B): Q[B] = pure(b)

  // Reinterpret any state in the Sign basis
  def toSign(b: Std): Q[Sign] = b match {
    case S0 => W(S_+ -> rhalf, S_- -> rhalf)
    case S1 => W(S_+ -> rhalf, S_- -> -rhalf)
  }

  def fromBasis[B <: Basis[B]](to: Std => Q[B])(s: B): Q[Std] = {
    val basis = List(S0, S1)
    W(basis.map(b => b -> to(b)(s).conj): _*)    
  }

  def fromSign(b: Sign): Q[Std] = {
    fromBasis(toSign)(b)
  }

  // Not gate
  def X(b: Std): Q[Std] = b match {
    case S0 => s1
    case S1 => s0
  }

  // Phase flip gate
  def Z(b: Std): Q[Std] = b match {
    case S0 => s0
    case S1 => -s1
  }

  // Hadamard gate
  def H(b: Std): Q[Std] = b match {
    case S0 => plus
    case S1 => minus
  }

  // Controlled not (CNOT) gate
  def cnot(b: T[Std, Std]): Q[T[Std, Std]] = b match {
    case T(S0, S0) => tensor(s0, s0)
    case T(S0, S1) => tensor(s0, s1)
    case T(S1, S0) => tensor(s1, s1)
    case T(S1, S1) => tensor(s1, s0)
  }

  // Rotation gate
  val tau = 2 * math.Pi
  def rot(theta: Double)(b: Std): Q[Std] = b match {
    case S0 => W(S0 -> math.cos(theta), S1 -> math.sin(theta))
    case S1 => W(S0 -> -math.sin(theta), S1 -> math.cos(theta))
  }

  // Square root of NOT gate
  val sqrtNot: U[Std] = rot(tau/8) _


  // Find the eigenvalues of an unitary operator
  def eigen(u: U[Std]) = {
    def solve(a: Complex, b: Complex, c: Complex): (Complex, Complex) = {
      val det = (b*b - 4*a*c)^(0.5)
      ((-b + det) / (2*a), (-b - det) / (2*a))
    }
    val a = u(S0)(S0)
    val b = u(S1)(S0)
    val c = u(S0)(S1)
    val d = u(S1)(S1)
    val (e1, e2) = solve(1, -(a+d), a*d - b*c)
    (e1, e2)
  }

  // Find the adjoint of a unitary transformation
  def adjoint[B <: Basis[B]](u: B => Q[B])(s: B): Q[B] = {
    val basis = s.vectors
    W(basis.map(b => b -> u(b)(s).conj): _*)
  }

  // Tensor product of two quantum states
  def tensor[B1 <: Basis[B1], B2 <: Basis[B2]](a: Q[B1], b: Q[B2]): Q[T[B1, B2]] = {
    for {
      x <- a
      y <- b
    } yield T(x, y)
  }

  // Lift 2 gates into a tensor product
  def lift12[B1 <: Basis[B1], B1a <: Basis[B1a], B2 <: Basis[B2], B2a <: Basis[B2a]](t1: B1 => Q[B1a], t2: B2 => Q[B2a])(s: T[B1, B2]): Q[T[B1a, B2a]] = {
    tensor(t1(s._1), t2(s._2))
  }

  // Lift a gate into the left side of a tensor product
  def lift1[B1 <: Basis[B1], B1a <: Basis[B1a], B2 <: Basis[B2]](t1: B1 => Q[B1a])(s: T[B1, B2]): Q[T[B1a, B2]] = {
    tensor(t1(s._1), pure(s._2))
  }

  // Lift a gate into the right side of a tensor product
  def lift2[B1 <: Basis[B1], B2 <: Basis[B2], B2a <: Basis[B2a]](t2: B2 => Q[B2a])(s: T[B1, B2]): Q[T[B1, B2a]] = {
    tensor(pure(s._1), t2(s._2))
  }

  val toSign12 = lift12(toSign, toSign) _

  // Re-associate a nested tensor product
  def assoc1[B1 <: Basis[B1], B2 <: Basis[B2], B3 <: Basis[B3]](b: T[B1, T[B2, B3]]): Q[T[T[B1, B2], B3]] = {
    b match { case T(b1, T(b2, b3)) => pure(T(T(b1, b2), b3)) }
  }

  // Re-associate a nested tensor product the other way
  def assoc2[B1 <: Basis[B1], B2 <: Basis[B2], B3 <: Basis[B3]](b: T[T[B1, B2], B3]): Q[T[B1, T[B2, B3]]] = {
    b match { case T(T(b1, b2), b3) => pure(T(b1, T(b2, b3))) }
  }
  
  // Flip the two sides of tensor product
  def flip[B1 <: Basis[B1], B2 <: Basis[B2]](b: T[B1, B2]): Q[T[B2, B1]] = {
    b match { case T(b1, b2) => pure(T(b2, b1)) }
  }
}

object Examples {
  import Complex._
  import W._
  import Basis._
  import Gate._

  def HZH(s: Q[Std]): Q[Std] = s >>= H >>= Z >>= H
  def runHZHequalsX(s: Q[Std]): (Q[Std], Q[Std]) = (HZH(s), s >>= X)

  // Some convenient states for testing
  val state1: Q[Std] = W(S0 -> 0.6, S1 -> 0.8.i)
  val state2: Q[Std] = W(S0 -> -0.5, S1 -> rquarter)

  def mkBell(s: T[Std, Std]): Q[T[Std, Std]] = pure(s) >>= lift1(H) >>= cnot
  def mkBell2(s: T[Std, Std]): Q[T[Std, Std]] = pure(s) >>= cnot >>= lift1(H)
  val mkBell3 = adjoint(mkBell) _
  val bell: Q[T[Std, Std]] = mkBell(T[Std, Std](S0, S0))
  val bell2: Q[T[Std, Std]] = mkBell(T[Std, Std](S0, S1))


  def runSqrtNot = s0 >>= sqrtNot >>= sqrtNot

  def teleport(alice: Q[Std]): (Boolean, Boolean, Q[Std]) = {
    val r = tensor(alice, bell) >>= assoc1 >>= lift1(cnot) >>= lift1(lift1(toSign))
    val (m, newState) = r.measure(_._1)
    val bit1 = m._2 == S0
    val bit2 = m._1 == S_+
    (bit1, bit2, newState.map(_._2))
  }
  
  def receive(bit1: Boolean, bit2: Boolean, bob: Q[Std]): Q[Std] = {
    val gate1: U[Std] = if (bit1) I _ else X _
    val gate2: U[Std] = if (bit2) I _ else Z _
    bob >>= gate1 >>= gate2
  }

  def runTeleport(alice: Q[Std]) {
    println("Alice's state: " + alice.toString)
    val (bit1, bit2, bob) = teleport(alice)
    println("Outcome of measurements: " + bit1 + ", " + bit2)
    println("Bob's state as a result of Alice's measurements: " + bob.toString)
    val r = receive(bit1, bit2, bob)
    println("Bob's state after applying gates: " + r.toString)
  }

  def runDecoherence {
    val simple = s0 >>= sqrtNot
    val entangled = bell

    println()
    println("** Without decoherence **")
    println("initial: " + simple.toString)
    val r1 = simple >>= sqrtNot
    println("rotate qubit: " + r1.toString)
    println("measure qubit: " + r1.measure()._1)
    println("measure qubit: " + r1.measure()._1)
    println("measure qubit: " + r1.measure()._1)
    println("measure qubit: " + r1.measure()._1)

    println()
    println("** With decoherence (entangled) **")
    println("initial: " + entangled.toString)
    val r2 = entangled >>= lift1(sqrtNot)
    println("rotate 1st qubit: " + r2.toString)
    println("measure 1st qubit: " + r2.measure(_._1)._1)
    println("measure 1st qubit: " + r2.measure(_._1)._1)
    println("measure 1st qubit: " + r2.measure(_._1)._1)
    println("measure 1st qubit: " + r2.measure(_._1)._1)

    println()
    println("Entangled qubit behaves like a classical random bit!")
  }
}
