package probability_monad

object Examples {
  import Distribution._

  /**
   * If you flip a coin and it comes up heads 10 times, what is the probability you have
   * the fair coin?
   */

  case class CoinTrial(haveFairCoin: Boolean, flips: List[Coin])
  def bayesianCoin(nflips: Int): Distribution[CoinTrial] = {
    for {
      haveFairCoin <- tf()
      val c = if (haveFairCoin) coin else biasedCoin(0.9)
      flips <- c.repeat(nflips)
    } yield CoinTrial(haveFairCoin, flips)
  }
  def runBayesianCoin(heads: Int) = bayesianCoin(heads).given(_.flips.forall(_ == H)).pr(_.haveFairCoin)


  /**
   * How many times do you need to flip a fair coin to get n heads in a row?
   */

  def expectedFlips(flips: Int): Distribution[Int] = {
    coin.until(cs => cs.length >= flips && cs.take(flips).forall(_ == H)).map(_.length)
  }


  /**
   * ELISA AIDS test. What is the probability you have the disease given a positive test result?
   */

  abstract sealed class Patient
  case object Sick extends Patient
  case object Well extends Patient
  def elisa: Distribution[(Patient, Boolean)] = {
    def test(patient: Patient) = patient match {
      case Sick => tf(0.997)
      case Well => tf(0.019)
    }
    val prevalence = 0.00448
    val person = discrete(List(Sick -> prevalence, Well -> (1-prevalence)))
    for {
      p <- person
      r <- test(p)
    } yield (p, r)
  }
  def runElisa = elisa.given(_._2).pr(_._1 == Sick)


  /**
   * If you flip a coin repeatedly, which is more likely to occur first, HTH or HTT?
   */

  def hth = coin.until(_.take(3) == List(H, T, H)).map(_.length)
  def htt = coin.until(_.take(3) == List(T, T, H)).map(_.length)


  /**
   * Given a biased coin with unknown bias distributed according to `prior`,
   * after flipping `nflips` times and observing `successes` heads,
   * what is the posterior distribution of the bias?
   */

  def unknownBiasedCoin(prior: Distribution[Double], nflips: Int, successes: Int): Distribution[Double] = {
    case class Trial(p: Double, successes: Int)
    val d = for {
      p <- prior
      flips <- tf(p).repeat(nflips)
    } yield Trial(p, flips.count(b => b))
    d.filter(_.successes == successes).map(_.p)
  }

  /**
   * Given a distribution `dist` with unknown parameter of type `A`, and a prior distribution `prior` of that parameter,
   * and given the `evidence` after conducting a `trial` using `dist`, what is the posterior distribution of the parameter?
   */

  def posterior[A, B, C](prior: Distribution[A])(dist: A => Distribution[B])(trial: Distribution[B] => Distribution[C])(evidence: C): Distribution[A] = {
    case class Trial(p: A, evidence: C)
    val d = for {
      p <- prior
      c <- trial(dist(p))
    } yield Trial(p, c)
    d.filter(_.evidence == evidence).map(_.p)
  }


  /**
   * RISK
   */

  // Attack once, return the number of matchups the attacker wins
  def attack(a: Int, d: Int): Distribution[Int] = {
    for {
      aDice <- dice(a min 3).map(_.sorted.reverse)
      dDice <- dice(d min 2).map(_.sorted.reverse)
    } yield aDice.zip(dDice).count{ case (a, b) => a > b }
  }

  // Attack until either A or D runs out of armies, return resulting number of armies on each side.
  def attacks(a: Int, d: Int): Distribution[(Int, Int)] = {
    if (a <= 1 || d == 0) always((a, d))
    else {
      for {
	r <- attack(a-1, d)
	s <- attacks(a - (2 - r), d - r)
      } yield s
    }
  }

  // Attacker attacks a series of territories until it runs out of armies, return the number
  // of territories conquered.
  def conquest(a: Int, ds: List[Int]): Distribution[Int] = {
    if (a <= 1) always(0)
    else {
      ds match {
        case Nil => always(0)
        case d::dd => for {
          (newA, newD) <- attacks(a, d)
          c <- conquest(newA-1, dd)
        } yield c + (if (newD == 0) 1 else 0)
      }
    }
  }
  def runConquest = conquest(20, List(3, 5, 2, 4)).hist


  /**
   * Each family has children until it has a boy, and then stops. What
   * is the expected fraction of girls in the population?
   */ 
  
  sealed abstract class Child
  case object Boy extends Child
  case object Girl extends Child
  def family = {
    discreteUniform(List(Boy, Girl)).until(_ contains Boy)
  }
  def population(families: Int): Distribution[Double] = {
    for {
      children <- family.repeat(families).map(_.flatten)
      val girls = children.count(_ == Girl)
    } yield girls.toDouble / children.length
  }
  def runBoyGirl = population(4).ev


  /**
   * A single bank teller can service a customer in 10 minutes. If one customer
   * comes in every 11 minutes on average, what is the expected length of the line?
   */

  def queue(loadFactor: Double): Distribution[Int] = {
    val incoming = poisson(loadFactor)
    markov(always(0), 100)(inLine => incoming.map(in => math.max(0, inLine + in - 1)))
  }
  def runBank = queue(0.9).map(_.toDouble).ev


  /**
   * You roll a 6-sided die and keep a running sum. What is the probability the
   * sum reaches exactly 30?
   */

  def dieSum(rolls: Int): Distribution[List[Int]] = {
    markov(always(List(0)), rolls)(runningSum => for {
      d <- die
    } yield (d + runningSum.head) :: runningSum)
  }
  def runDieSum = dieSum(30).pr(_ contains 30)

  /**
   * Random walk: starting at 0 and moving left or right with equal probability,
   * how many steps do you expect to take before reaching 10?
   */

  def randomWalk(target: Int, maxSteps: Int): Distribution[List[Int]] = {
    markov(always(List(0)))(steps => steps.head == target || steps.length == maxSteps, positions => for {
      direction <- discreteUniform(List(-1, 1))
    } yield (positions.head + direction) :: positions)
  }
  def runRandomWalk = randomWalk(10, 1000).map(_.length.toDouble).ev

  /**
   * Pascal's triangle
   */

  def pascal(depth: Int): Distribution[(Int, Int)] = {
    markov(always((0, 0)), depth){ case (left, right) => for {
      moveLeft <- tf()
    } yield {
      if (moveLeft) (left+1, right) else (left, right+1)
    }}
  }
  def runPascal = pascal(6).hist


  /**
   * Simpson's Paradox
   */

  abstract class Party
  case object Democrat extends Party
  case object Republican extends Party

  abstract class State
  case object North extends State
  case object South extends State

  def simpson(): Distribution[(Party, State, Boolean)] = {

    def stateToParty(state: State) = state match {
      case North => discrete(List(Democrat -> 154.0, Republican -> 162.0))
      case South => discrete(List(Democrat -> 94.0, Republican -> 1.0))
    }

    def votedFor(party: Party, state: State): Distribution[Boolean] = {
      (party, state) match {
        case (Democrat, North) => tf(0.94)
        case (Democrat, South) => tf(0.07)
        case (Republican, North) => tf(0.85)
        case (Republican, South) => tf(0.01)
      }
    }
    val senators = discrete(List(
      (Democrat, North) -> 154.0,
      (Democrat, South) -> 94.0,
      (Republican, North) -> 162.0,
      (Republican, South) -> 1.0
    ))
    for {
      (party, state) <- senators
      vote <- votedFor(party, state)
    } yield (party, state, vote)
  }
  def runSimpsonDem() = simpson().given(_._1 == Democrat).pr(_._3)
  def runSimpsonRep() = simpson().given(_._1 == Republican).pr(_._3)


  /**
   * Monty Hall problem
   */

  val montyHall = {
    val doors = (1 to 3).toSet
    for {
      prize <- discreteUniform(doors)   // The prize is placed randomly
      choice <- discreteUniform(doors)  // You choose randomly
      opened <- discreteUniform(doors - prize - choice)   // Monty opens one of the other doors
      switch <- discreteUniform(doors - choice - opened)  // You switch to the unopened door
    } yield (prize, switch)
  }
  def runMontyHall = montyHall.pr{ case (prize, switch) => prize == switch }


  /**
   * Mr. Jones has two children. The older child is a girl.
   * What is the probability that both children are girls?
   */ 

  val jones = {
    discreteUniform(List(Boy, Girl))
      .repeat(2)
      .filter(_.head == Girl)
  }
  def runJones = jones.pr(_.forall(_ == Girl)) // 0.5
  
  /**
   * Mr. Smith has two children. At least one of them is a boy.
   * What is the probability that both children are boys?
   */

  val smith = {
    discreteUniform(List(Boy, Girl))
      .repeat(2)
      .filter(_ contains Boy)
  }
  def runSmith = smith.pr(_.forall(_ == Boy)) // 0.333

  /**
   * Mr. Miller has two children. One of them is a boy born on Tuesday.
   * What is the probability both children are boys?
   */

  val tuesday = {
    val child = for {
      sex <- discreteUniform(List(Boy, Girl))
      day <- d(7)
    } yield (sex, day)
    child.repeat(2).filter(_ contains (Boy, 3))
  }
  def runTuesday = tuesday.pr(_.forall(_._1 == Boy)) // 0.47


  /**
   * Bayesian networks
   */

  sealed trait BloodGene
  case object A_ extends BloodGene
  case object B_ extends BloodGene
  case object O_ extends BloodGene

  sealed trait BloodType
  case object A extends BloodType
  case object B extends BloodType
  case object AB extends BloodType
  case object O extends BloodType

  implicit object BloodTypeOrd extends Ordering[BloodType] {
    override def compare(a: BloodType, b: BloodType) = {
      if (a == b) 0
      else {
        (a, b) match {
          case (A, _) => -1
          case (_, A) => 1
          case (B, _) => -1
          case (_, B) => 1
          case (AB, _) => -1
          case (_, AB) => 1
          case _ => 1
        }
      }
    }
  }

  val bloodPrior: Distribution[(BloodGene, BloodGene)] = {
    for {
      g1 <- discreteUniform(List(A_, B_, O_))
      g2 <- discreteUniform(List(A_, B_, O_))
    } yield (g1, g2)
  }

  def typeFromGene(g: (BloodGene, BloodGene)): Distribution[BloodType] = {
    g match {
      case (A_, A_) => always(A)
      case (A_, B_) => always(AB)
      case (A_, O_) => always(A)
      case (B_, A_) => always(AB)
      case (B_, B_) => always(B)
      case (B_, O_) => always(B)
      case (O_, A_) => always(A)
      case (O_, B_) => always(B)
      case (O_, O_) => always(O)
    }
  }

  def childFromParents(p1: (BloodGene, BloodGene), p2: (BloodGene, BloodGene)): Distribution[(BloodGene, BloodGene)] = {
    val (p1a, p1b) = p1
    val (p2a, p2b) = p2
    discreteUniform(for {
      p1 <- List(p1a, p1b)
      p2 <- List(p2a, p2b)
    } yield (p1, p2))
  }

  case class BloodTrial(bart: BloodType, lisa: BloodType, homer: BloodType, marge: BloodType, selma: BloodType, jackie: BloodType)
  val bloodType = for {
    gHomer <- bloodPrior
    bHomer <- typeFromGene(gHomer)
    gHarry <- bloodPrior
    gJackie <- bloodPrior
    bJackie <- typeFromGene(gJackie)
    gSelma <- childFromParents(gHarry, gJackie)
    bSelma <- typeFromGene(gSelma)
    gMarge <- childFromParents(gHarry, gJackie)
    bMarge <- typeFromGene(gMarge)
    gBart <- childFromParents(gHomer, gMarge)
    bBart <- typeFromGene(gBart)
    gLisa <- childFromParents(gHomer, gMarge)
    bLisa <- typeFromGene(gLisa)
  } yield BloodTrial(bBart, bLisa, bHomer, bMarge, bSelma, bJackie)

  def runBloodType = bloodType.filter(_.selma == A).pr(_.bart == A)

  /**
   * Teasing apart correlation and causality.
   * From http://www.michaelnielsen.org/ddi/if-correlation-doesnt-imply-causation-then-what-does/
   *
   * First, observe the joint probability distribution of:
   *  - whether someone smokes
   *  - whether someone has tar in their lungs
   *  - whether someone gets cancer
   *
   * Encode this as a graphical model.
   */

  // 50% of the population smokes (made up numbers)
  def smoker: Distribution[Boolean] = tf(0.5)

  /**
   * 95% of smokers have tar in their lungs
   * 5% of nonsmokers have tar in their lungs
   */
  def tar(smoker: Boolean): Distribution[Boolean] = {
    if (smoker) tf(0.95)
    else tf(0.05)
  }

  /**
   * Observed probabilities of getting cancer broken out by
   * whether you smoke and whether you have tar in your lungs
   */
  def cancer(smoker: Boolean, tar: Boolean): Distribution[Boolean] = {
    (smoker, tar) match {
      case (false, false) => tf(0.1)
      case (true, false) => tf(0.9)
      case (false, true) => tf(0.05)
      case (true, true) => tf(0.85)
    }
  }

  case class SmokingTrial(smoker: Boolean, tar: Boolean, cancer: Boolean) {
    override def toString = {
      "smoker=%-5s tar=%-5s cancer=%-5s".format(smoker, tar, cancer)
    }
  }
  implicit val SmokingTrialOrdering = Ordering.by((t: SmokingTrial) => (t.smoker, t.tar, t.cancer))

  /**
   * This encodes the probability distribution of smoking and cancer.
   * We can use it to calculate p(cancer) and p(cancer|smoking).
   */
  def smoking: Distribution[SmokingTrial] = {
    for {
      s <- smoker
      t <- tar(s)
      c <- cancer(s, t)
    } yield SmokingTrial(s, t, c)
  }

  /*
   * According to the article, this encodes the graphical model
   * that models the situation where we force some people to smoke
   * to see if they get cancer (i.e., p(cancer|do(smoker))), using
   * only observational data.
   *
   * The intuition behind this graphical model is still unclear to me.
   */
  def doSmoking: Distribution[SmokingTrial] = {
    for {
      s1 <- smoker
      s2 <- smoker
      t <- tar(s1)
      c <- cancer(s2, t)
    } yield SmokingTrial(s1, t, c)
  }

  def runSmoking = {
    println("p(cancer) = " + smoking.pr(_.cancer))
    println("p(cancer|smoking) = " + smoking.pr(_.cancer, _.smoker))
    println("p(cancer|do(smoking)) = " + doSmoking.pr(_.cancer, _.smoker))
    println()
    println("Since p(cancer|do(smoking)) < p(cancer), smoking actually prevents cancer (according to our made-up numbers)")
    println("even though, naively, p(cancer|smoking) > p(cancer)!")
  }


  // Experimental...
  object Smoking {
    def hidden = tf(0.3)
    def smoker(hidden: Boolean) = hidden match {
      case true => tf(0.7)
      case false => tf(0.3)
    }
    def tar(smoker: Boolean): Distribution[Boolean] = smoker match {
      case true => tf(0.95)
      case false => tf(0.05)
    }
    def cancer(hidden: Boolean, tar: Boolean): Distribution[Boolean] = {
      (hidden, tar) match {
        case (true, true) => tf(0.7)
        case (true, false) => tf(0.9)
        case (false, true) => tf(0.05)
        case (false, false) => tf(0.1)
      }
    }

    val natural = for {
      h <- hidden
      s <- smoker(h)
      t <- tar(s)
      c <- cancer(h, t)
    } yield SmokingTrial(s, t, c)

    def doSmoker = for {
      h <- hidden
      s <- smoker(h)
    } yield s

    val controlled = for {
      h <- hidden
      s <- doSmoker
      t <- tar(s)
      c <- cancer(h, t)
    } yield SmokingTrial(s, t, c)

    val doSmoking = {
      val model = natural
      def smoker = {
        model.map(_.smoker)
      }
      def tar(smoker: Boolean) = {
        model.filter(_.smoker == smoker).map(_.tar)
      }
      def cancer(smoker: Boolean, tar: Boolean) = {
        model.filter(_.smoker == smoker).filter(_.tar == tar).map(_.cancer)
      }

      for {
        s1 <- smoker
        s2 <- smoker
        t <- tar(s1)
        c <- cancer(s2, t)
      } yield SmokingTrial(s1, t, c)
    }
  }

  object Test {
    case class Trial(a: Boolean, b1: Boolean, b2: Boolean, c: Boolean)
    def hidden = tf(0.3)
    def A(hidden: Boolean) = hidden match {
      case true => tf(0.7)
      case false => tf(0.3)
    }
    def B1(a: Boolean) = a match {
      case true => tf(0.95)
      case false => tf(0.75)
    }
    def B2(a: Boolean) = a match {
      case true => tf(0.3)
      case false => tf(0.2)
    }
    def C(hidden: Boolean, b1: Boolean, b2: Boolean) = (hidden, b1, b2) match {
      case (true, true, true) => tf(0.8)
      case (true, true, false) => tf(0.9)
      case (true, false, true) => tf(0.6)
      case (true, false, false) => tf(0.7)
      case (false, true, true) => tf(0.4)
      case (false, true, false) => tf(0.5)
      case (false, false, true) => tf(0.2)
      case (false, false, false) => tf(0.3)
    }

    val natural = for {
      h <- hidden
      a <- A(h)
      b1 <- B1(a)
      b2 <- B2(a)
      c <- C(h, b1, b2)
    } yield Trial(a, b1, b2, c)

    val doA = for {
      h <- hidden
      a <- A(h)
    } yield a

    val controlled = for {
      h <- hidden
      a <- doA
      b1 <- B1(a)
      b2 <- B2(a)
      c <- C(h, b1, b2)
    } yield Trial(a, b1, b2, c)

    val doTest = {
      val model = natural
      def A = model.map(_.a)
      def B1(a: Boolean) = model.filter(_.a == a).map(_.b1)
      def B2(a: Boolean) = model.filter(_.a == a).map(_.b2)
      def C(a: Boolean, b1: Boolean, b2: Boolean) = {
        model.filter(_.a == a).filter(_.b1 == b1).filter(_.b2 == b2).map(_.c)
      }

      for {
        a1 <- A
        a2 <- A
        b1 <- B1(a1)
        b2 <- B2(a1)
        c <- C(a2, b1, b2)
      } yield Trial(a1, b1, b2, c)
    }
  }

  /**
   * The probablistic graphical model
   *
   *      Y -> Q
   *      |
   *      v
   * X -> Z -> W
   *
   */

  def X = tf(0.4)
  def Y = tf(0.6)
  def Z(x: Boolean, y: Boolean) = (x, y) match {
    case (true, true) => tf(0.2)
    case (true, false) => tf(0.7)
    case (false, true) => tf(0.5)
    case (false, false) => tf(0.8)
  }
  def W(z: Boolean) = z match {
    case true => tf(0.3)
    case false => tf(0.9)
  }
  def Q(y: Boolean) = y match {
    case true => tf(0.6)
    case false => tf(0.2)
  }

  case class Trial(x: Boolean, y: Boolean, z: Boolean, w: Boolean, q: Boolean)
  def pgm = {
    for {
      x <- X
      y <- Y
      z <- Z(x, y)
      w <- W(z)
      q <- Q(y)
    } yield Trial(x, y, z, w, q)
  }

  def dep[A, B](p: Distribution[A])(e1: A => B, e2: A => B)(implicit ord: Ordering[B]) = {
    p.map(t => (e1(t), e2(t)))
     .histData.toList
     .groupBy{ case ((e1, e2), pr) => e1 }
     .mapValues(vs => {
        vs.map{ case ((e1, e2), pr) => (e2, pr) }
     })
     .toList.sortBy(_._1)(ord)
     .foreach{ case (e1, e2prs) => {
       val total = e2prs.map(_._2).sum
       println()
       println("%s (%.2f%%)".format(e1, total * 100))
       e2prs.sortBy(_._1)(ord)
         .foreach{ case (e2, pr) => {
           println("  %s: %.2f%%".format(e2.toString, pr / total * 100))
         }}
     }}
  }

  def doPGM {
    println()
    println("X and Y are independent: p(y|x) = ")
    dep(pgm)(_.x, _.y)

    println()
    println("Given knowledge of Z, X and Y are dependent: p(y|x, z=true) = ")
    dep(pgm.filter(_.z == true))(_.x, _.y)

    println()
    println("Given knowledge of W, X and Y are dependent: p(y|x, w=false) = ")
    dep(pgm.filter(_.w == false))(_.x, _.y)

    println()
    println("X and W are dependent: p(w|x) = ")
    dep(pgm)(_.x, _.w)

    println()
    println("Given knowledge of Z, X and W are independent: p(w|x, z=true) = ")
    dep(pgm.filter(_.z == true))(_.x, _.w)

    println()
    println("Z and Q are dependent: p(q|z) = ")
    dep(pgm)(_.z, _.q)

    println()
    println("Given knowledge of Y, Z and Q are independent: p(q|z, y=true) = ")
    dep(pgm.filter(_.y == true))(_.z, _.q)
  }

  def centralLimitTheorem1(d: Distribution[Double], samples: Int) = {
    println()
    println("Original distribution:")
    d.bucketedHist(20)

    println()
    val stdev = d.stdev
    println("mean = " + d.mean)
    println("stdev = " + stdev)

    println()
    println("Distribution of means of samples of size %d:".format(samples))
    val sd = d.repeat(samples).map(xs => xs.sum / samples)
    sd.bucketedHist(20)

    println()
    println("This distribution is always a normal distribution regardless of the shape of the original distribution.")
    println("Empirically, the mean and stdev are:")
    println("mean = " + sd.mean)
    println("stdev = " + sd.stdev)

    println()
    println("However, we can also compute the stdev directly from the original distribution:")
    println("stdev = s / sqrt(n) = " + stdev / math.sqrt(samples))
  }

  def runCentralLimitTheorem1 = centralLimitTheorem1(uniform, 100)

  def centralLimitTheorem2(d: Distribution[Boolean], samples: Int) = {
    val p = d.pr(x => x)
    println()
    println("p = pr(b = true) = " + p)

    println()
    println("Distribution of pr(b = true) of samples of size %d:".format(samples))
    val sd = d.repeat(samples).map(xs => xs.count(x => x).toDouble / samples)
    sd.bucketedHist(20)

    println()
    println("This distribution is always a normal distribution.")
    println("Empirically, the mean and stdev are:")
    println("mean = " + sd.mean)
    println("stdev = " + sd.stdev)

    println()
    println("However, we can also compute the stdev directly from the original distribution:")
    println("stdev = sqrt(p*(1-p)/n) = " + math.sqrt(p * (1 - p) / samples))
  }

  def runCentralLimitTheorem2 = centralLimitTheorem2(tf(0.2), 100)

  def centralLimitTheorem3(d1: Distribution[Double], d2: Distribution[Double], samples1: Int, samples2: Int) = {
    println()
    println("Original distribution 1:")
    d1.bucketedHist(20)

    println()
    val mean1 = d1.mean
    val stdev1 = d1.stdev
    println("mean = " + mean1)
    println("stdev = " + stdev1)

    println()
    println("Original distribution 2:")
    d2.bucketedHist(20)
    println()
    val mean2 = d2.mean
    val stdev2 = d2.stdev
    println("mean = " + mean2)
    println("stdev = " + stdev2)

    println()
    println("Distribution of difference of means of samples of size %d and %d:".format(samples1, samples2))
    val sd1 = d1.repeat(samples1).map(xs => xs.sum / xs.size)
    val sd2 = d2.repeat(samples2).map(xs => xs.sum / xs.size)
    val sd = sd1 - sd2
    sd.bucketedHist(20)

    println()
    println("This distribution is always a normal distribution regardless of the shapes of the original distributions.")
    println("Empirically, the mean and stdev are:")
    println("mean = " + sd.mean)
    println("stdev = " + sd.stdev)

    println()
    println("However, we can also compute the mean and stdev directly from the original distributions:")
    println("mean = mean1 - mean2 = " + (d1.mean - d2.mean))
    println("stdev = sqrt( stdev1^2 / n1 + stdev2^2 / n2 ) = " + math.sqrt(stdev1 * stdev1 / samples1 + stdev2 * stdev2 / samples2))
  }

  def runCentralLimitTheorem3 = centralLimitTheorem3(normal, uniform, 100, 200)
}
