package probability_monad

import java.math.MathContext
import scala.annotation.tailrec
import scala.math.BigDecimal
import scala.util.Random

trait Distribution[A] {
  self =>
  protected def get: A

  override def toString = "<distribution>"

  def map[B](f: A => B): Distribution[B] = new Distribution[B] {
    override def get = f(self.get)
  }

  def flatMap[B](f: A => Distribution[B]): Distribution[B] = new Distribution[B] {
    override def get = f(self.get).get
  }

  def filter(pred: A => Boolean): Distribution[A] = new Distribution[A] {
    @tailrec
    override def get = {
      val s = self.get
      if (pred(s)) s else this.get
    }
  }

  def given(pred: A => Boolean): Distribution[A] = filter(pred)

  def until(pred: List[A] => Boolean): Distribution[List[A]] = new Distribution[List[A]] {
    override def get = {
      @tailrec
      def helper(sofar: List[A]): List[A] = {
	if (pred(sofar)) sofar
	else helper(self.get :: sofar)
      }
      helper(Nil)
    }
  }

  def repeat(n: Int): Distribution[List[A]] = until(_.length == n)

  @tailrec
  final def iterate(n: Int, f: A => Distribution[A]): Distribution[A] = {
    if (n == 0) this
    else this.flatMap(f).iterate(n-1, f)
  }

  def iterateUntil(pred: A => Boolean, f: A => Distribution[A]): Distribution[A] = new Distribution[A] {
    override def get = {
      @tailrec
      def helper(a: A): A = {
        if (pred(a)) a
        else helper(f(a).get)
      }
      helper(self.get)
    }
  }
  
  private val N = 10000

  def pr(pred: A => Boolean, given: A => Boolean = (a: A) => true, samples: Int = N): Double = {
    1.0 * this.filter(given).sample(samples).count(pred) / samples
  }

  // NB: Expected value only makes sense for real-valued distributions. If you want to find the expected
  // value of a die roll, for example, you have to do die.map(_.toDouble).ev.
  def ev(implicit f: Fractional[A]): A = f.div(Stream.fill(N)(self.get).sum, f.fromInt(N))

  def mean(implicit f: Fractional[A]): A = ev

  def variance(implicit f: Fractional[A]): A = {
    val mean = this.mean
    this.map(x => {
      val d = f.minus(x, mean)
      f.times(d, d)
    }).ev
  }

  def stdev(implicit f: Fractional[A]): Double = math.sqrt(f.toDouble(variance))

  def skewness(implicit f: Fractional[A]): Double = {
    val mean = f.toDouble(this.mean)
    val stdev = this.stdev
    this.map(a => {
      val x = f.toDouble(a)
      val d = (x - mean) / stdev
      d * d * d
    }).ev
  }

  def kurtosis(implicit f: Fractional[A]): A = {
    val mean = this.mean
    val variance = this.variance
    f.div(this.map(x => {
      val d = f.minus(x, mean)
      val d2 = f.times(d, d)
      f.times(d2, d2)
    }).ev, f.times(variance, variance))
  }

  def sample(n: Int = N): List[A] = List.fill(n)(self.get)

  def zip[B](d: Distribution[B]): Distribution[(A, B)] = new Distribution[(A, B)] {
    override def get = (self.get, d.get)
  }

  def zipWith[B, C](d: Distribution[B])(f: (A, B) => C): Distribution[C] = new Distribution[C] {
    override def get = f(self.get, d.get)
  }

  def +(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.plus(self.get, d.get)
  }
  def -(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.minus(self.get, d.get)
  }
  def *(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.times(self.get, d.get)
  }
  def /(d: Distribution[A])(implicit f: Fractional[A]): Distribution[A] = new Distribution[A] {
    override def get = f.div(self.get, d.get)
  }

  def hist = {
    this.sample(N).groupBy(x=>x).mapValues(_.length.toDouble / N)
  }

  def plotHist(implicit ord: Ordering[A] = null) = {
    val histogram = hist.toList
    val sorted = if (ord == null) histogram else histogram.sortBy(_._1)(ord)
    doPlot(sorted)
  }

  private def findBucketWidth(min: Double, max: Double, buckets: Int): (BigDecimal, BigDecimal, BigDecimal, Int) = {
    // Use BigDecimal to avoid annoying rounding errors.
    val widths = List(0.1, 0.2, 0.25, 0.5, 1.0, 2.0, 2.5, 5.0, 10.0).map(BigDecimal.apply)
    val span = max - min
    val p = (math.log(span) / math.log(10)).toInt - 1
    val scale = BigDecimal(10).pow(p)
    val scaledWidths = widths.map(_ * scale)
    val bestWidth = scaledWidths.minBy(w => (span / w - buckets).abs)
    val outerMin = (min / bestWidth).toInt * bestWidth
    val outerMax = ((max / bestWidth).toInt + 1) * bestWidth
    val actualBuckets = ((outerMax - outerMin) / bestWidth).toInt
    (outerMin, outerMax, bestWidth, actualBuckets)
  }

  def plotBucketedHist(buckets: Int = 20)(implicit ord: Ordering[A], frac: Fractional[A]) = {
    val data = this.sample(N).toList.sorted
    val min = data.head
    val max = data.last
    val (outerMin, outerMax, width, nbuckets) = findBucketWidth(frac.toDouble(min), frac.toDouble(max), buckets)
    val rm = BigDecimal.RoundingMode.HALF_UP
    def toBucket(a: A): BigDecimal = ((frac.toDouble(a) - outerMin) / width).setScale(0, rm) * width + outerMin
    val bucketed = data
      .groupBy(toBucket)
      .mapValues(_.size.toDouble / N)
      .toList.sortBy(_._1)
    doPlot(bucketed)
  }

  private def doPlot[B](data: Iterable[(B, Double)]) = {
    val scale = 100
    val maxWidth = data.map(_._1.toString.length).max
    val fmt = "%"+maxWidth+"s %5.2f%% %s"
    data.foreach{ case (a, p) => {
      val hashes = (p * scale).toInt
      println(fmt.format(a.toString, p*100, "#" * hashes))
    }}    
  }
}

object Distribution {
  private val rand = new Random()

  def always[A](value: A) = new Distribution[A] {
    override def get = value
  }

  object u extends Distribution[Double] {
    override def get = rand.nextDouble()
  }

  object normal extends Distribution[Double] {
    override def get = rand.nextGaussian()
  }

  sealed abstract class Coin
  case object H extends Coin
  case object T extends Coin
  def coin: Distribution[Coin] = discreteUniform(List(H, T))
  def biasedCoin(p: Double): Distribution[Coin] = discrete(List(H -> p, T -> (1-p)))

  def d(n: Int) = discreteUniform(1 to n)
  def die = d(6)
  def dice(n: Int) = die.repeat(n)
  
  def tf(p: Double = 0.5) = discrete(List(true -> p, false -> (1-p)))

  def uniform(lo: Double = 0.0, hi: Double = 1.0) = u.map(x => (x * (hi - lo)) + lo)

  def discreteUniform[A](values: Iterable[A]): Distribution[A] = new Distribution[A] {
    private val vec = Vector() ++ values
    override def get = vec(rand.nextInt(vec.length))
  }

  def discrete[A](weightedValues: Iterable[(A, Double)]): Distribution[A] = new Distribution[A] {
    val len = weightedValues.size
    val scale = len / weightedValues.map(_._2).sum
    val scaled = weightedValues.map{ case (a, p) => (a, p * scale) }.toList
    val (smaller, bigger) = scaled.partition(_._2 < 1.0)
    private def alias(smaller: List[(A, Double)], bigger: List[(A, Double)]): List[(A, Double, Option[A])] = {
      smaller match {
        case Nil => bigger.map{ case (a, _) => (a, 1.0, None) }
        case (s, sp)::ss => {
          val (b, pb)::bb = bigger
          val remainder = (b, pb - (1.0 - sp))
          val rest = if (remainder._2 < 0.9999) alias(remainder :: ss, bb) else alias(ss, remainder :: bb)
          (s, sp, Some(b)) :: rest
        }
      }
    }
    val table = Vector() ++ alias(smaller, bigger)
    private def select(p1: Double, p2: Double, table: Vector[(A, Double, Option[A])]): A = {
      table((p1 * len).toInt) match {
        case (a, _, None) => a
        case (a, p, Some(b)) => if (p2 <= p) a else b
      }
    }
    override def get = {
      select(u.get, u.get, table)
    }
  }

  def poisson(lambda: Double): Distribution[Int] = new Distribution[Int] {
    override def get = {
      val m = math.exp(-lambda)
      val s = Stream.iterate(1.0)(_ * u.get)
      s.tail.takeWhile(_ > m).length
    }
  }

  /**
   * Turn any CDF into a Distribution[Double]
   */
  def fromCDF(cdf: Double => Double): Distribution[Double] = {
    /**
     * Inverts a monotone increasing function via binary search
     */
    @annotation.tailrec
    def invert(f: Double => Double, y: Double, min: Double, max: Double): Double = {
      val x = (min + max) / 2
      val fx = f(x)
      if (math.abs(y - fx) < 0.0001) {
        x
      } else if (y > f(max)) {
        invert(f, y, max, max * 2)
      } else if (y < f(min)) {
        invert(f, y, min * 2, min)
      } else if (y > fx) {
        invert(f, y, x, max)
      } else { // if (y < fx)
        invert(f, y, min, x)
      }
    }

    uniform().map(y => invert(cdf, y, -1.0, 1.0))
  }

  def pareto(a: Double, xm: Double = 1.0) = fromCDF(x => {
    if (x < xm) 0.0
    else 1 - math.pow(xm / x, a)
  })

  def exponential(l: Double) = fromCDF(x => {
    1 - math.exp(-1 * l * x)
  })

  def lognormal = normal.map(math.exp)

  def zipf(s: Double, n: Int) = {
    discrete(List.tabulate(n)(k => (k+1, 1.0 / math.pow(k+1, s))))
  }

  def binomial(p: Double, n: Int): Distribution[Int] = {
    tf(p).repeat(n).map(_.count(b => b))
  }

  def chi2(n: Int) = List.fill(n)(normal).map(x => x*x).reduceLeft[Distribution[Double]](_ + _)

  lazy val cauchy = normal / normal

  def markov[A](init: Distribution[A], steps: Int)(transition: A => Distribution[A]): Distribution[A] = {
    init.iterate(steps, transition)
  }

  def markov[A](init: Distribution[A])(stop: A => Boolean, transition: A => Distribution[A]): Distribution[A] = {
    init.iterateUntil(stop, transition)
  }
}
