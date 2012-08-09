import Complex._

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

  def filter(f: A => Boolean)(implicit num: Fractional[B]): W[A, B] = {
    W(state.filter{ case (a, b) => f(a) }: _*).normalize
  }

  // Make sure the sum of the squares of the coefficients is 1
  def normalize(implicit frac: Fractional[B]) = {
    val total = math.sqrt(state.map{ case (a, b) => num2.norm(b) }.sum)
    this.scale(1 / total)
  }

  // Measure a quantum state (or a part of one). Returns the result of the measurement and the new state.
  def measure[A1](w: A => A1)(implicit frac: Fractional[B]): (A1, W[A, B]) = {
    val r = rand.nextDouble()
    def find(r: Double, s: List[(A1, Double)]): A1 = s match {
      case (l, p) :: Nil => l
      case (l, p) :: rest if r < p => l
      case (l, p) :: rest => find(r - p, rest)
      case Nil => throw new Exception("empty state")
    }
    val squaredAmplitudes = this.map(w).state.toList.map{ case (a, b) => (a, num2.norm(b)) }
    val measurement = find(r, squaredAmplitudes)
    val newState = this.filter(s => w(s) == measurement)
    (measurement, newState)
  }

  def inner(other: W[A, B]) {
    val m = other.state.toMap
    W((for {
      (l, v1) <- state
      v2 <- m.get(l)
    } yield (l, num2.conj(v1) * v2)) :_*)
  }

  override def toString = {
    state.filter{ case (a, b) => num2.norm(b) > 0.00001 }.map{ case (a, b) => b.toString + a.toString }.mkString(" ")
  }
}

class WIsNumeric[A, B](implicit num: Numeric[B], num2: Numeric2[B]) extends Numeric[W[A, B]] {
  private def ??? = throw new Exception
  override def compare(x: W[A, B], y: W[A, B]) = if (x.state == y.state) 0 else 1
  override def fromInt(x: Int) = ???
  override def plus(x: W[A, B], y: W[A, B]) = W(x.state ++ y.state :_*).collect
  override def minus(x: W[A, B], y: W[A, B]) = plus(x, negate(y))
  override def negate(x: W[A, B]) = x.scale(-1)
  override def times(x: W[A, B], y: W[A, B]) = ???
  override def toDouble(x: W[A, B]) = ???
  override def toFloat(x: W[A, B]) = ???
  override def toInt(x: W[A, B]) = ???
  override def toLong(x: W[A, B]) = ???
}

type Q[A] = W[A, Complex]

object W {
  implicit def wIsNumeric[A, B: Numeric: Numeric2]: WIsNumeric[A, B] = new WIsNumeric[A, B]
  val rhalf: Complex = math.sqrt(0.5)
  val rquarter: Complex = math.sqrt(0.75)
  def apply[A, B: Numeric: Numeric2](state: (A, B)*) = new W(state: _*)
  def pure[A](a: A): Q[A] = W(a -> one)
}

class Basis(val label: String) {
  override def toString = "|"+label+">"
}

object Basis {
  import W._

  // Standard basis { |0>, |1> }
  sealed abstract class Std(label: String) extends Basis(label)
  case object S0 extends Std("0")
  case object S1 extends Std("1")

  val s0: Q[Std] = pure(S0)
  val s1: Q[Std] = pure(S1)
  val plus: Q[Std] = W(S0 -> rhalf, S1 -> rhalf)
  val minus: Q[Std] = W(S0 -> rhalf, S1 -> rhalf * -1)

  // Sign basis { |+>, |-> }
  sealed abstract class Sign(label: String) extends Basis(label)
  case object S_+ extends Sign("+")
  case object S_- extends Sign("-")

  val s_+ = pure(S_+)
  val s_- = pure(S_-)

  // Tensor product of two bases, e.g., T[Std, Std] = { |00>, |01>, |10>, |11> }
  case class T[+B1 <: Basis, +B2 <: Basis](_1: B1, _2: B2) extends Basis(_1.label + _2.label)
}

object Gate {
  import W.{pure, rhalf}
  import Basis._
  
  // A unitary transformation (a quantum gate)
  type U[B <: Basis] = B => Q[B]

  // Identity gate
  def I[B <: Basis](b: B): Q[B] = pure(b)

  // Reinterpret any state in the Sign basis
  def toSign(b: Std): Q[Sign] = b match {
    case S0 => W(S_+ -> rhalf, S_- -> rhalf)
    case S1 => W(S_+ -> rhalf, S_- -> rhalf * -1)
  }

  // Not gate
  def X(b: Std): Q[Std] = b match {
    case S0 => s1
    case S1 => s0
  }

  // Phase flip gate
  def Z(b: Std): Q[Std] = b match {
    case S0 => s0
    case S1 => s1.scale(-1)
  }

  // Hadamard get
  def H(b: Std): Q[Std] = b match {
    case S0 => plus
    case S1 => minus
  }

  // Controlled not (CNOT) gate
  def cnot(b: T[Std, Std]): Q[T[Std, Std]] = b match {
    case T(S0, S0) => pure(T(S0, S0))
    case T(S0, S1) => pure(T(S0, S1))
    case T(S1, S0) => pure(T(S1, S1))
    case T(S1, S1) => pure(T(S1, S0))
  }

  // Find the adjoint of a unitary transformation
  def adjoint[B](u: Std => Q[B])(b: B): Q[Std] = {
    W(S0 -> u(S0)(b).conj, S1 -> u(S1)(b).conj)
  }

  def adjoint2(u: T[Std, Std] => Q[T[Std, Std]])(s: T[Std, Std]): Q[T[Std, Std]] = {
    val basis = List(T(S0, S0), T(S0, S1), T(S1, S0), T(S1, S1))
    W(basis.map(b => b -> u(b)(s).conj): _*)
  }

  val fromSign = adjoint(toSign) _

  // Tensor product of two quantum states
  def tensor[B1 <: Basis, B2 <: Basis](a: Q[B1], b: Q[B2]): Q[T[B1, B2]] = {
    for {
      x <- a
      y <- b
    } yield T(x, y)
  }

  // Lift 2 gates into a tensor product
  def lift12[B1 <: Basis, B1a <: Basis, B2 <: Basis, B2a <: Basis](t1: B1 => Q[B1a], t2: B2 => Q[B2a])(s: T[B1, B2]): Q[T[B1a, B2a]] = {
    tensor(t1(s._1), t2(s._2))
  }

  // Lift a gate into the left side of a tensor product
  def lift1[B1 <: Basis, B1a <: Basis, B2 <: Basis](t1: B1 => Q[B1a])(s: T[B1, B2]): Q[T[B1a, B2]] = {
    tensor(t1(s._1), pure(s._2))
  }

  // Lift a gate into the right side of a tensor product
  def lift2[B1 <: Basis, B2 <: Basis, B2a <: Basis](t2: B2 => Q[B2a])(s: T[B1, B2]): Q[T[B1, B2a]] = {
    tensor(pure(s._1), t2(s._2))
  }

  val toSign12 = lift12(toSign, toSign) _

  // Re-associate a nested tensor product
  def assoc1[B1 <: Basis, B2 <: Basis, B3 <: Basis](b: T[B1, T[B2, B3]]): Q[T[T[B1, B2], B3]] = {
    b match { case T(b1, T(b2, b3)) => pure(T(T(b1, b2), b3)) }
  }

  // Re-associate a nested tensor product the other way
  def assoc2[B1 <: Basis, B2 <: Basis, B3 <: Basis](b: T[T[B1, B2], B3]): Q[T[B1, T[B2, B3]]] = {
    b match { case T(T(b1, b2), b3) => pure(T(b1, T(b2, b3))) }
  }
  
  // Flip the two sides of tensor product
  def flip[B1 <: Basis, B2 <: Basis](b: T[B1, B2]): Q[T[B2, B1]] = {
    b match { case T(b1, b2) => pure(T(b2, b1)) }
  }
}

object Examples {
  import W._
  import Basis._
  import Gate._

  def HZH(s: Q[Std]): Q[Std] = s >>= H >>= Z >>= H
  def runHZHequalsX(s: Q[Std]): (Q[Std], Q[Std]) = (HZH(s), s >>= X)

  // Some convenient states for testing
  val state1: Q[Std] = W(S0 -> 0.6, S1 -> 0.8.i)
  val state2: Q[Std] = W(S0 -> -0.5, S1 -> rquarter)

  val bell: Q[T[Std, Std]] = W(T(S0, S0) -> rhalf, T(S1, S1) -> rhalf)
  val bell2: Q[T[Std, Std]] = W(T(S0, S1) -> rhalf, T(S1, S0) -> rhalf * -1)
  def bell3(s: T[Std, Std]): Q[T[Std, Std]] = pure(s) >>= lift1(H) >>= cnot
  def bell4(s: T[Std, Std]): Q[T[Std, Std]] = pure(s) >>= cnot >>= lift1(H)
  val bell5 = adjoint2(bell3) _

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

  def runTeleport(alice: Q[Std]): Q[Std] = {
    val (bit1, bit2, bob) = teleport(alice)
    receive(bit1, bit2, bob)
  }
}

import Basis._
import Gate._
import Examples._

