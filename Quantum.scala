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

abstract class Basis(val label: String) {
  override def toString = "|"+label+">"
}

trait Convertable[B1 <: Basis, B2 <: Basis] {
  def convert(b: B1): W.Q[B2]
}

trait Enumerable[B <: Basis] {
  def vectors: List[B]
}

object Enumerable {
  import Basis._
  implicit object StdIsEnumerable extends Enumerable[Std] {
    override val vectors = List(S0, S1)
  }
}

object Convertable {
  import W._
  import Basis._

  implicit object SignIsConvertable extends Convertable[Std, Sign] {
    def convert(b: Std): Q[Sign] = b match {
      case S0 => W(S_+ -> rhalf, S_- -> rhalf)
      case S1 => W(S_+ -> rhalf, S_- -> -rhalf)
    }
  }

  implicit def id[B <: Basis]: Convertable[B, B] = new Convertable[B, B] {
    override def convert(b: B): Q[B] = pure(b)
  }

  implicit def sym[B1 <: Basis, B2 <: Basis](implicit from: Convertable[B2, B1], enum: Enumerable[B2]): Convertable[B1, B2] = new Convertable[B1, B2] {
    override def convert(b1: B1): Q[B2] = {
      W(enum.vectors.map(b2 => b2 -> from.convert(b2)(b1).conj): _*)
    }
  }

/*
  implicit def trans[B1 <: Basis, B2 <: Basis, B3 <: Basis](implicit c12: Convertable[B1, B2], c23: Convertable[B2, B3]): Convertable[B1, B3] = new Convertable[B1, B3] {
    override def convert(b1: B1): Q[B3] = pure(b1) >>= c12.convert >>= c23.convert
  }
*/

  def convert[B1 <: Basis, B2 <: Basis](implicit c: Convertable[B1, B2]): B1 => Q[B2] = c.convert _
}

object Basis {
  import W._

  // Standard basis { |0>, |1> }
  abstract sealed class Std(label: String) extends Basis(label)
  case object S0 extends Std("0")
  case object S1 extends Std("1")

  val s0: Q[Std] = pure(S0)
  val s1: Q[Std] = pure(S1)
  val plus: Q[Std] = W(S0 -> rhalf, S1 -> rhalf)
  val minus: Q[Std] = W(S0 -> rhalf, S1 -> -rhalf)

  // Sign basis { |+>, |-> }
  abstract sealed class Sign(label: String) extends Basis(label)
  case object S_+ extends Sign("+")
  case object S_- extends Sign("-")

  val s_+ = pure(S_+)
  val s_- = pure(S_-)

  // Tensor product of two bases, e.g., T[Std, Std] = { |00>, |01>, |10>, |11> }
  case class T[+B1 <: Basis, +B2 <: Basis](_1: B1, _2: B2) extends Basis(_1.label + _2.label)

  object T {
    implicit def TIsConvertable[B1 <: Basis, B2 <: Basis, B3 <: Basis, B4 <: Basis](implicit c1: Convertable[B1, B3], c2: Convertable[B2, B4]): Convertable[T[B1, B2], T[B3, B4]] = new Convertable[T[B1, B2], T[B3, B4]] {
      def convert(b: T[B1, B2]) = pure(b) >>= Gate.lift12(c1.convert, c2.convert)
      }
    
    implicit def TIsEnumerable[B1 <: Basis, B2 <: Basis](implicit e1: Enumerable[B1], e2: Enumerable[B2]): Enumerable[T[B1, B2]] = new Enumerable[T[B1, B2]] {
      override val vectors = {
	for {
	  b1 <- e1.vectors
	  b2 <- e2.vectors
	} yield T(b1, b2)
      }
    }
  }

  case class L[B <: Basis](ls: List[B]) extends Basis(ls.map(_.label).mkString) {
    val n = ls.length
    val N = math.pow(2, n).toInt
    def splitAt(n: Int) = {
      val (a, b) = ls.splitAt(n)
      (L(a), L(b))
    }
  }

  object L {

    def fromInt(i: Int, width: Int): L[Std] = {
      def helper(i: Int, width: Int, acc: List[Std]): List[Std] = {
	if (width == 0) acc
	else helper(i / 2, width-1, (if (i % 2 == 0) S0 else S1) :: acc)
      }
      new L(helper(i, width, Nil))
    }

    def toInt(s: L[Std]): Int = {
      def helper(ls: List[Std], acc: Int): Int = {
	ls match {
	  case Nil => acc
	  case S0 :: rest => helper(rest, acc * 2)
	  case S1 :: rest => helper(rest, acc * 2 + 1)
	}
      }
      helper(s.ls, 0)
    }
  }
}

object Gate {
  import W.{pure, rhalf, Q}
  import Basis._
  
  // A unitary transformation (a quantum gate)
  type U[B <: Basis] = B => Q[B]

  // Identity gate
  def I[B <: Basis](b: B): Q[B] = pure(b)

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

  // Implementation of f(x) as a quantum gate
  def U(f: Int => Int, width: Int)(s: L[Std]): Q[L[Std]] = {
    val (in, out) = s.ls.splitAt(width)
    val fx = L.fromInt(f(L.toInt(L(in))) ^ L.toInt(L(out)), out.length)
    tensorLL(pure(L(in)), pure(fx))
  }

  // Quantum Fourier Transform
  // correct, but should be implemented in terms of smaller instances of QFT and basic gates
  def QFT(b: L[Std]): Q[L[Std]] = {
    val w = Complex.polar(1.0, tau / b.N)
    val base = w ^ L.toInt(b)
    W((0 until b.N).map(i => L.fromInt(i, b.n) -> (base ^ i)): _*).normalize
  }

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
  def adjoint[B1 <: Basis, B2 <: Basis, B3 <: Basis](u: B1 => Q[B2])(implicit c31: Convertable[B3, B1], c23: Convertable[B2, B3], e3: Enumerable[B3]): B2 => Q[B1] = {
    def adjointEnum(u: B3 => Q[B3])(b3: B3): Q[B3] = {
      val basis = e3.vectors
      W(basis.map(b => b -> u(b)(b3).conj): _*)
    }
    (b2: B2) => pure(b2) >>= c23.convert >>= adjointEnum({ (b3: B3) => pure(b3) >>= c31.convert >>= u >>= c23.convert }) >>= c31.convert
  }

  // Tensor product of two quantum states
  def tensor[B1 <: Basis, B2 <: Basis](a: Q[B1], b: Q[B2]): Q[T[B1, B2]] = {
    for {
      x <- a
      y <- b
    } yield T(x, y)
  }

  def tensorL[B <: Basis](a: Q[B], b: Q[L[B]]): Q[L[B]] = {
    for {
      x <- a
      y <- b
    } yield L(x :: y.ls)
  }

  def tensorLL[B <: Basis](a: Q[L[B]], b: Q[L[B]]): Q[L[B]] = {
    for {
      x <- a
      y <- b
    } yield L(x.ls ++ y.ls)
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

  def liftL[B1 <: Basis, B2 <: Basis](t: B1 => Q[B2])(s: L[B1]): Q[L[B2]] = {
    s match {
      case L(Nil) => pure(L(Nil))
      case L(s0 :: rest) => tensorL(t(s0), liftL(t)(L(rest)))
    }
  }

  def liftSlice[B <: Basis](t: L[B] => Q[L[B]], start: Int, len: Int)(s: L[B]): Q[L[B]] = {
    val (pre, rest) = s.ls.splitAt(start)
    val (mid, post) = rest.splitAt(len)
    tensorLL(tensorLL(pure(L(pre)), t(L(mid))), pure(L(post)))
  }

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
  import Complex._
  import W._
  import Basis._
  import Gate._
  import Convertable._

  def HZH(s: Q[Std]): Q[Std] = s >>= H >>= Z >>= H
  def runHZHequalsX(s: Q[Std]): (Q[Std], Q[Std]) = (HZH(s), s >>= X)

  // Some convenient states for testing
  val state1: Q[Std] = W(S0 -> 0.6, S1 -> 0.8.i)
  val state2: Q[Std] = W(S0 -> -0.5, S1 -> rquarter)

  def mkBell(s: T[Std, Std]): Q[T[Std, Std]] = pure(s) >>= lift1(H) >>= cnot
  def mkBell2(s: T[Std, Std]): Q[T[Std, Std]] = pure(s) >>= cnot >>= lift1(H)
  val mkBell3: U[T[Std, Std]] = adjoint(mkBell) _
  val bell: Q[T[Std, Std]] = mkBell(T(S0, S0))
  val bell2: Q[T[Std, Std]] = mkBell(T(S0, S1))


  def runSqrtNot = s0 >>= sqrtNot >>= sqrtNot

  def teleport(alice: Q[Std]): (Boolean, Boolean, Q[Std]) = {
    val r = tensor(alice, bell) >>= assoc1 >>= lift1(cnot) >>= lift1(lift1(convert[Std, Sign]))
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

  /**
   * Grover's algorithm
   */
  def grover(f: Int => Int, width: Int) = {
    val Hn = liftSlice(liftL(H), 0, width) _
    val minusL = pure(L.fromInt(1, 1)) >>= liftL(H)
    val init = tensorLL(pure(L.fromInt(0, width)), minusL) >>= Hn
    val inv = U(f, width) _
    def g(x: Int): Int = if (x == 0) 0 else 1
    def refl(s: L[Std]) = pure(s) >>= Hn >>= U(g, width) >>= Hn

    (0 to (math.pow(2, width / 2 + 1)).toInt).foldLeft(init){ case (s, _) => s >>= inv >>= refl }
  }
  def runGrover = {
    def f(x: Int) = if (x == 14) 1 else 0
    val s = grover(f, 4)
    println("final state: " + s.toString)
    val m = L.toInt(s.measure(_.splitAt(4)._1)._1)
    println("measurement: " + m)
  }

  /**
   * Shor's quantum factorization algorithm (TODO)
   */
  def findPeriod(f: Int => Int, width: Int) = {
    def trial = {
      val s1 = pure(L.fromInt(0, width * 2)) >>= liftSlice(QFT, 0, width) >>= U(f, width) >>= liftSlice(QFT, 0, width)
      L.toInt(s1.measure(_.splitAt(width)._1)._1)
    }
    def gcd(a: Int, b: Int): Int = {
      if (b == 0) a
      else gcd(b, a % b)
    }
    val r = List.fill(30)(trial).reduceLeft(gcd)
    math.pow(2, width).toInt / r
  }
  def runFindPeriod = {
    def f(x: Int) = x % 4 + 1
    findPeriod(f, 5)
  }
}
