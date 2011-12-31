object Examples {
  import Distribution._

  /**
   * If you flip a coin and it comes up heads 10 times, what is the probability you have
   * the fair coin?
   */

  def bayesianCoin(flips: Int) = {
    for {
      whichCoin <- d(2)
      val c = if (whichCoin == 1) coin else biasedCoin(0.9)
      results <- c.repeat(flips)
    } yield (whichCoin, results)
  }
  def runBayesianCoin(heads: Int) = bayesianCoin(heads).given(_._2.forall(_ == H)).p(_._1 == 1)


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
  def runElisa = elisa.given(_._2).p(_._1 == Sick)


  /**
   * If you flip a coin repeatedly, which is more likely to occur first, HTH or HTT?
   */

  def hth = coin.until(_.take(3) == List(H, T, H)).map(_.length)
  def htt = coin.until(_.take(3) == List(H, T, T)).map(_.length)


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
}
