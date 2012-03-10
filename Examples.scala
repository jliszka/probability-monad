object Examples {
  import Distribution._

  /**
   * If you flip a coin and it comes up heads 10 times, what is the probability you have
   * the fair coin?
   */

  def bayesianCoin(flips: Int) = {
    for {
      haveFairCoin <- tf()
      val c = if (haveFairCoin) coin else biasedCoin(0.9)
      results <- c.repeat(flips)
    } yield (haveFairCoin, results)
  }
  def runBayesianCoin(heads: Int) = bayesianCoin(heads).given(_._2.forall(_ == H)).pr(_._1)


  /**
   * How many times do you need to flip a fair coin to get n heads in a row?
   */

  def expectedFlips(flips: Int) = {
    coin.until(cs => cs.length >= flips && cs.take(flips).forall(_ == H)).map(_.length)
  }


  /**
   * ELISA AIDS test. What is the probability you have the disease given a positive test result?
   */

  abstract sealed class Patient
  case object Sick extends Patient
  case object Well extends Patient
  def elisa = {
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
   * RISK
   **/

  // Attack once, return the number of matchups the attacker wins
  def attack(a: Int, d: Int) = {
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
   * is the expected fraction of of girls in the population?
   */ 
  
  sealed abstract class Child
  case object Boy extends Child
  case object Girl extends Child
  def family = {
    discreteUniform(List(Boy, Girl)).until(_.exists(_ == Boy))
  }
  def population(families: Int) = {
    for {
      children <- family.repeat(families).map(_.flatten)
      val girls = children.count(_ == Girl)
    } yield 1.0 * girls / children.length
  }
  def runBoyGirl = population(4).ev


  /**
   * A single bank teller can service a customer in 10 minutes. If one customer
   * comes in every 11 minutes on average, what is the expected length of the line?
   */

  def queue(loadFactor: Double) = {
    val incoming = poisson(loadFactor)
    markov(0, 100)(inLine => incoming.map(in => math.max(0, inLine + in - 1)))
  }
  def runBank = queue(0.9)


  /**
   * You roll a 6-sided die and keep a running sum. What is the probability the
   * sum reaches exactly 30?
   */

  def dieSum(rolls: Int): Distribution[List[Int]] = {
    markov(List(0), rolls)(runningSum => for {
      d <- die
    } yield (d + runningSum.head) :: runningSum)
  }
  def runDieSum = dieSum(30).pr(_.contains(30))

  /**
   * Random walk: starting at 0 and moving left or right with equal probability,
   * how many steps do you expect to take before reaching 10?
   */

  def randomWalk(target: Int, maxSteps: Int): Distribution[List[Int]] = {
    markov(List(0))(steps => steps.head == target || steps.length == maxSteps, positions => for {
      direction <- discreteUniform(List(-1, 1))
    } yield (positions.head + direction) :: positions)
  }
  def runRandomWalk = randomWalk(10, 1000).map(_.length.toDouble).ev

  /**
   * Pascal's triangle
   */

  def pascal(depth: Int): Distribution[(Int, Int)] = {
    markov((0, 0), depth){ case (left, right) => for {
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
}
