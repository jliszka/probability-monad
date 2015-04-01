package probability_monad

import java.util.concurrent.ThreadLocalRandom

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParSeq
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

  def withFilter(pred: A => Boolean): Distribution[A] = filter(pred)

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

  def repeat(n: Int): Distribution[List[A]] = new Distribution[List[A]] {
    override def get = self.sample(n)
  }

  /**
   * Using this distribution as a prior, compute the posterior distribution after running an experiment
   * and observing some outcomes and not others.
   */
  def posterior[B](experiment: A => Distribution[B])(observed: B => Boolean): Distribution[A] = {
    case class Trial(p: A, evidence: B)
    val d = for {
      p <- this
      e <- experiment(p)
    } yield Trial(p, e)
    d.filter(t => observed(t.evidence)).map(_.p)
  }

  /**
   * Markov chains
   */

  @tailrec
  final def markov(n: Int)(f: A => Distribution[A]): Distribution[A] = {
    if (n == 0) this
    else this.flatMap(f).markov(n-1)(f)
  }

  def markov(pred: A => Boolean)(f: A => Distribution[A]): Distribution[A] = new Distribution[A] {
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
    1.0 * this.filter(given).samplePar(samples).count(pred) / samples
  }

  // NB: Expected value only makes sense for real-valued distributions. If you want to find the expected
  // value of a die roll, for example, you have to do die.map(_.toDouble).ev.
  def ev(implicit toDouble: A <:< Double): Double = {
    (0 until N).par.map(_ => toDouble(self.get)).aggregate(0d)(_ + _ / N, _ + _)
  }

  def mean(implicit toDouble: A <:< Double): Double = ev

  private def square(x: Double) = x * x
  private def cube(x: Double) = x * x * x

  def variance(implicit toDouble: A <:< Double): Double = {
    val mean = this.mean
    this.map(x => {
      square(toDouble(x) - mean)
    }).ev
  }

  def stdev(implicit toDouble: A <:< Double): Double = {
    math.sqrt(this.variance)
  }

  def skewness(implicit toDouble: A <:< Double): Double = {
    val mean = this.mean
    val stdev = this.stdev
    this.map(x => {
      cube((toDouble(x) - mean) / stdev)
    }).ev
  }

  def kurtosis(implicit toDouble: A <:< Double): Double = {
    val mean = this.mean
    val variance = this.variance
    this.map(x => {
      square(square(toDouble(x) - mean))
    }).ev / square(variance)
  }

  def sample(n: Int = N): List[A] = List.fill(n)(self.get)

  def samplePar(n: Int = N): ParSeq[A] = (0 until N).par.map(i => self.get)

  /**
   * "Freeze" a distribution by taking a sample and serving values out of that sample at random.
   * Useful for when a distribution is expensive to compute and is being sampled from repeatedly.
   */
  def freeze: Distribution[A] = {
    Distribution.discreteUniform(sample(N*10))
  }

  def zip[B](d: Distribution[B]): Distribution[(A, B)] = new Distribution[(A, B)] {
    override def get = (self.get, d.get)
  }

  def zipWith[B, C](d: Distribution[B])(f: (A, B) => C): Distribution[C] = new Distribution[C] {
    override def get = f(self.get, d.get)
  }

  def +(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.plus(self.get, d.get)
  }
  def +(x: A)(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.plus(self.get, x)
  }
  def -(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.minus(self.get, d.get)
  }
  def -(x: A)(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.minus(self.get, x)
  }
  def *(d: Distribution[A])(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.times(self.get, d.get)
  }
  def *(x: A)(implicit n: Numeric[A]): Distribution[A] = new Distribution[A] {
    override def get = n.times(self.get, x)
  }
  def /(d: Distribution[A])(implicit toDouble: A <:< Double): Distribution[Double] = new Distribution[Double] {
    override def get = toDouble(self.get) / toDouble(d.get)
  }
  def /(x: A)(implicit toDouble: A <:< Double): Distribution[Double] = new Distribution[Double] {
    override def get = toDouble(self.get) / toDouble(x)
  }

  def hist(implicit ord: Ordering[A] = null, d: A <:< Double = null) = {
    if (d == null) {
      plotHist(ord)
    } else {
      bucketedHist(20)(ord, d)
    }
  }

  def histData: Map[A, Double] = {
    this.sample(N).groupBy(x=>x).mapValues(_.length.toDouble / N)
  }

  private def plotHist(implicit ord: Ordering[A] = null) {
    val histogram = this.histData.toList
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

  def bucketedHist(buckets: Int)(implicit ord: Ordering[A], toDouble: A <:< Double) {
    val data = this.sample(N).toList.sorted
    val min = data.head
    val max = data.last
    val (outerMin, outerMax, width, nbuckets) = findBucketWidth(toDouble(min), toDouble(max), buckets)
    bucketedHistHelper(outerMin, outerMax, nbuckets, data, roundDown = false)(ord, toDouble)
  }

  def bucketedHist(min: Double, max: Double, nbuckets: Int, roundDown: Boolean = false)
                  (implicit ord: Ordering[A], toDouble: A <:< Double) {
    val data = this.sample(N).filter(a => {
      val x = toDouble(a)
      min <= x && x <= max
    }).sorted
    bucketedHistHelper(BigDecimal(min), BigDecimal(max), nbuckets, data, roundDown)(ord, toDouble)
  }

  private def bucketedHistHelper(min: BigDecimal, max: BigDecimal, nbuckets: Int, data: List[A], roundDown: Boolean)
                  (implicit ord: Ordering[A], toDouble: A <:< Double) {
    val rm = if (roundDown) BigDecimal.RoundingMode.DOWN else BigDecimal.RoundingMode.HALF_UP
    val width = (max - min) / nbuckets
    def toBucket(a: A): BigDecimal = ((toDouble(a) - min) / width).setScale(0, rm) * width + min
    val n = data.size
    val bucketToProb = data
      .groupBy(toBucket)
      .mapValues(_.size.toDouble / n)
    val bucketed = (min to max by width).map(a => a -> bucketToProb.getOrElse(a, 0.0))
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
  private val rand = ThreadLocalRandom.current()

  def always[A](value: A) = new Distribution[A] {
    override def get = value
  }

  /**
   * Discrete distributions
   */

  sealed abstract class Coin
  case object H extends Coin
  case object T extends Coin
  def coin: Distribution[Coin] = discreteUniform(List(H, T))
  def biasedCoin(p: Double): Distribution[Coin] = discrete(H -> p, T -> (1-p))

  def d(n: Int) = discreteUniform(1 to n)
  def die = d(6)
  def dice(n: Int) = die.repeat(n)

  def tf(p: Double = 0.5) = discrete(true -> p, false -> (1-p))

  def bernoulli(p: Double = 0.5) = discrete(1 -> p, 0 -> (1-p))

  def discreteUniform[A](values: Iterable[A]): Distribution[A] = new Distribution[A] {
    private val vec = Vector() ++ values
    override def get = vec(rand.nextInt(vec.length))
  }

  def discrete[A](weightedValues: (A, Double)*): Distribution[A] = new Distribution[A] {
    val len = weightedValues.size
    val scale = len / weightedValues.map(_._2).sum
    val scaled = weightedValues.map{ case (a, p) => (a, p * scale) }.toList
    val (smaller, bigger) = scaled.partition(_._2 < 1.0)

    // The alias method: http://www.keithschwarz.com/darts-dice-coins/
    @tailrec
    private def alias(smaller: List[(A, Double)], bigger: List[(A, Double)], rest: List[(A, Double, Option[A])]): List[(A, Double, Option[A])] = {
      (smaller, bigger) match {
        case ((s, sp) :: ss, (b, pb) :: bb) =>
          val remainder = (b, pb - (1.0 - sp))
          val newRest = (s, sp, Some(b)) :: rest
          if (remainder._2 < 1)
            alias(remainder :: ss, bb, newRest)
          else
            alias(ss, remainder :: bb, newRest)
        case (_, (b, pb) :: bb) =>
          alias(smaller, bb, (b, 1.0, None) :: rest)
        case ((s, sp) :: ss, _) =>
          alias(ss, bigger, (s, 1.0, None) :: rest)
        case _ =>
          rest
      }
    }
    val table = Vector() ++ alias(smaller, bigger, Nil)
    private def select(p1: Double, p2: Double, table: Vector[(A, Double, Option[A])]): A = {
      table((p1 * len).toInt) match {
        case (a, _, None) => a
        case (a, p, Some(b)) => if (p2 <= p) a else b
      }
    }
    override def get = {
      select(uniform.get, uniform.get, table)
    }
  }

  def geometric(p: Double): Distribution[Int] = {
    tf(p).until(_.headOption == Some(true)).map(_.size - 1)
  }

  def binomial(p: Double, n: Int): Distribution[Int] = {
    bernoulli(p).repeat(n).map(_.sum)
  }

  def negativeBinomial(p: Double, r: Int): Distribution[Int] = {
    tf(p).until(_.count(_ == false) == r).map(_.size - r)
  }

  def poisson(lambda: Double): Distribution[Int] = {
    exponential(1).until(_.sum > lambda).map(_.size - 1)
  }

  def zipf(s: Double, n: Int): Distribution[Int] = {
    discrete((1 to n).map(k => k -> 1.0 / math.pow(k, s)): _*)
  }

  /**
   * Continuous distributions
   */

  object uniform extends Distribution[Double] {
    override def get = rand.nextDouble()
  }

  object normal extends Distribution[Double] {
    override def get = rand.nextGaussian()
  }

  def chi2(n: Int): Distribution[Double] = {
    normal.map(x => x*x).repeat(n).map(_.sum)
  }

  def students_t(df: Int): Distribution[Double] = {
    for {
      z <- normal
      v <- chi2(df)
    } yield z * math.sqrt(df / v)
  }

  def pareto(a: Double, xm: Double = 1.0): Distribution[Double] = {
    for {
      x <- uniform
    } yield xm * math.pow(x, -1/a)
  }

  def exponential(l: Double): Distribution[Double] = {
    for {
      x <- uniform
    } yield math.log(x) / (-l)
  }

  def laplace(b: Double): Distribution[Double] = {
    val d = exponential(1/b)
    d - d
  }

  def F(d1: Int, d2: Int): Distribution[Double] = {
    chi2(d1) / chi2(d2)
  }

  def lognormal: Distribution[Double] = {
    for {
      z <- normal
    } yield math.exp(z)
  }

  def cauchy: Distribution[Double] = {
    normal / normal
  }

  def weibull(l: Double, k: Double): Distribution[Double] = {
    for {
      y <- exponential(1)
    } yield l * math.pow(y, 1/k)
  }

  def gamma(k: Double, theta: Double): Distribution[Double] = {
    val n = k.toInt
    val gammaInt = uniform.repeat(n).map(_.map(x => -math.log(x)).sum)
    val gammaFrac = {
      val delta = k - n
      // From https://en.wikipedia.org/wiki/Gamma_distribution#Generating_gamma-distributed_random_variables
      def helper(): Distribution[Double] = {
        for {
          u1 <- uniform
          u2 <- uniform
          u3 <- uniform
          (zeta, eta) = {
            val v0 = math.E / (math.E + delta)
            if (u1 <= v0) {
              val zeta = math.pow(u2, 1/delta)
              val eta = u3 * math.pow(zeta, delta - 1)
              (zeta, eta)
            } else {
              val zeta = 1 - math.log(u2)
              val eta = u3 * math.exp(-zeta)
              (zeta, eta)
            }
          }
          r <- if (eta > math.pow(zeta, delta - 1) * math.exp(-zeta)) helper() else always(zeta)
        } yield r
      }
      helper()
    }
    (gammaInt + gammaFrac) * theta
  }

  def beta(a: Double, b: Double): Distribution[Double] = {
    for {
      x <- gamma(a, 1)
      y <- gamma(b, 1)
    } yield x / (x + y)
  }

  def sequence[T](ds: List[Distribution[T]]): Distribution[List[T]] = new Distribution[List[T]] {
    override def get = ds.map(_.get)
  }

  def dirichlet(alphas: List[Double]): Distribution[List[Double]] = {
    sequence(alphas.map(a => gamma(a, 1))).map(ys => {
      val sum = ys.sum
      ys.map(_ / sum)
    })
  }

  /**
   * Tests if two probability distributions are the same using the Kolmogorov-Smirnov test.
   * The distributions are unlikely to be the same (p < 0.05) if the value is greater than 1.35
   * and very unlikely (p < 0.001) if the value is greater than 1.95.
   */
  def ksTest[A](d1: Distribution[A], d2: Distribution[A])(implicit ord: Ordering[A]): Double = {
    val n = 100000
    val d1s = d1.sample(n).sorted.zipWithIndex
    val d2s = d2.sample(n).sorted.zipWithIndex
    val all = (d1s ++ d2s).sorted.zipWithIndex
    // 2i is the expected index in the combined list and j is the actual index.
    val worstOffset = all.map{ case ((x, i), j) => math.abs(2 * i - j) }.max / 2
    val ksStatistic = worstOffset.toDouble / n
    ksStatistic / math.sqrt(2.0 * n / (n * n))
  }

  /**
   * Determine if a joint probability distribution is composed of 2 independent events.
   * Uses the G-test: http://en.wikipedia.org/wiki/G-test
   */
  def chi2test[A, B](d: Distribution[(A, B)]): Double = {
    val data = d.histData
    val total = data.map(_._2).sum
    val rowValues = data.map(_._1._1).toSet
    val colValues = data.map(_._1._2).toSet

    val rowTotals = (for {
      row <- rowValues
    } yield row -> colValues.map(col => data.getOrElse((row, col), 0.0)).sum).toMap

    val colTotals = (for {
      col <- colValues
    } yield col -> rowValues.map(row => data.getOrElse((row, col), 0.0)).sum).toMap

    val chi2stat = (for {
      row <- rowValues
      col <- colValues
    } yield {
      val observed = data.getOrElse((row, col), 0.0)
      val expected = {
        val rowTotal = rowTotals.getOrElse(row, 0.0)
        val colTotal = colTotals.getOrElse(col, 0.0)
        rowTotal.toDouble * colTotal / total
      }
      observed * math.log(observed / expected)
    }).sum * 2
    val df = (rowValues.size - 1) * (colValues.size - 1)
    chi2(df).pr(_ > chi2stat)
  }
}
