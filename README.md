# Probability Distribution Monad

Makes it easy to create, manipulate and sample probability distributions.

For example, here's how you would code up the following problem: You are given either a fair coin or a
biased coin with equal probability. If you flip it 5 times and it comes up heads each time, what is the
probability you have the fair coin?

    case class Trial(haveFairCoin: Boolean, flips: List[Coin])

    def bayesianCoin(nflips: Int): Distribution[Trial] = {
      for {
        haveFairCoin <- tf()
        c = if (haveFairCoin) coin else biasedCoin(0.9)
        flips <- c.repeat(nflips)
      } yield Trial(haveFairCoin, flips)
    }
  
    bayesianCoin(5).given(_.flips.forall(_ == H)).pr(_.haveFairCoin)

Or: You repeatedly roll a 6-sided and keep a running sum. What is the probability the sum reaches
exactly 30?

    def dieSum(rolls: Int): Distribution[List[Int]] = {
      markov(rolls, List(0))(runningSum => for {
        d <- die
      } yield (d + runningSum.head) :: runningSum)
    }

    dieSum(30).pr(_ contains 30)

Or: Each family has children until it has a boy, and then stops. What is the expected fraction of girls in the population?

    sealed abstract class Child
    case object Boy extends Child
    case object Girl extends Child

    def family = {
      discreteUniform(List(Boy, Girl)).until(_ contains Boy)
    }

    def population(families: Int) = {
      for {
        children <- family.repeat(families).map(_.flatten)
        val girls = children.count(_ == Girl)
      } yield 1.0 * girls / children.length
    }

    population(4).ev

[Distribution.scala](https://github.com/jliszka/probability-monad/blob/master/Distribution.scala) contains code
for creating and manipulating probability distributions. Built-in distributions include:

- uniform continuous 
- uniform discrete (including die and fair coin)
- weighted discrete (biased coin, uses the [alias method](http://www.keithschwarz.com/darts-dice-coins/))
- normal
- poisson
- binomial
- cauchy
- chi2

Methods for manipulating distributions include:

- adding (convolution), subracting (cross-correlation), multiplying and dividing distributions
- producing joint distributions from single distributions
- conditional distributions (amounts to a filter)
- mapping and flatMapping values inside the distribution
- creating Markov chains
- finding the probability of arbitrary predicates, conditional probabililty
- finding expected values
- sampling, histogram

[Examples.scala](https://github.com/jliszka/probability-monad/blob/master/Examples.scala) contains some 
example uses, and possibly a RISK simulator.

To try out some examples, do

    $ ./run.sh

    scala> runBayesianCoin(5)

Contributions welcome!
