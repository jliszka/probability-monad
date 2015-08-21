# Probability Distribution Monad

Makes it easy to create, manipulate and sample probability distributions.

## Installation

Include this in your sbt config:

    "org.jliszka" %% "probability-monad" % "1.0.1"

## Examples

Here's how you would code up the following problem: You are given either a fair coin or a
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

Or: You repeatedly roll a 6-sided die and keep a running sum. What is the probability the sum reaches
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

## How it works

A ```Distribution[T]``` represents a random variable that, when sampled, produces values of type ```T``` according
to a particular probability distribution. For example, ```Distribution.uniform``` is a ```Distribution[Double]```
that produces ```Double``` values between 0.0 and 1.0, uniformly distributed. ```Distribution.coin``` is a
```Distribution[Coin]``` that produces the values ```H``` and ```T``` with equal probability, and
```Distribution.biasedCoin(0.3)``` is a ```Distribution[Coin]``` that produces the value ```H``` 30% of the time
and the value ```T``` 70% of the time.

You can think of a ```Distribution[T]``` as a collection like any other scala collection that you can ```map```,
```flatMap``` and ```filter``` over. The presence of these methods allow you to use scala's for-comprehensions to manipulate
distributions. For example, here's how you would create a distribution that represents the sum of 2 die rolls:

    val dice = for {
      d1 <- die
      d2 <- die
    } yield d1 + d2

Here, ```die``` is a ```Distribution[Int]```, and ```d1``` and ```d2``` are both ```Int```s. The type of ```dice```
is ```Distribution[Int]```. You can see that for-comprehensions are an easy way to define new a distribution in terms of individual
samples from other distributions.

You can visualize a distribution with ```hist```:

    scala> dice.hist
     2  2.61% ##
     3  5.48% #####
     4  8.70% ########
     5 10.53% ##########
     6 14.21% ##############
     7 16.90% ################
     8 13.90% #############
     9 11.43% ###########
    10  8.35% ########
    11  5.17% #####
    12  2.72% ##

If you want more control over the display of continuous distributions, use ```bucketedHist```:

    scala> normal.map(_ * 2 + 1).bucketedHist(20)   // 20 buckets, min & max determined automatically
    -7.0  0.02%
    -6.0  0.03%
    -5.0  0.22%
    -4.0  1.01% #
    -3.0  2.69% ##
    -2.0  6.43% ######
    -1.0 12.19% ############
     0.0 17.07% #################
     1.0 19.74% ###################
     2.0 17.55% #################
     3.0 12.17% ############
     4.0  6.55% ######
     5.0  2.92% ##
     6.0  1.10% #
     7.0  0.23%
     8.0  0.06%
     9.0  0.01%
    10.0  0.01%

    scala> cauchy.bucketedHist(-10, 10, 20)   // min=-10, max=10, #buckets=20
    -10.0  0.20%
     -9.0  0.38%
     -8.0  0.44%
     -7.0  0.55%
     -6.0  0.82%
     -5.0  1.23% #
     -4.0  1.85% #
     -3.0  2.92% ##
     -2.0  6.78% ######
     -1.0 16.78% ################
      0.0 30.04% ##############################
      1.0 16.64% ################
      2.0  6.22% ######
      3.0  3.06% ###
      4.0  1.76% #
      5.0  1.26% #
      6.0  0.84%
      7.0  0.67%
      8.0  0.48%
      9.0  0.42%
     10.0  0.14%


This probability monad is based on sampling, so the values and plots produced will be inexact and will vary between runs.

    scala> normal.stdev
    res9: Double = 1.0044818262040809

    scala> normal.stdev
    res10: Double = 1.0071194147525722

# Code and examples

[Distribution.scala](https://github.com/jliszka/probability-monad/blob/master/src/main/scala/probability-monad/Distribution.scala) contains code
for creating and manipulating probability distributions. Built-in distributions include:

- uniform discrete (including die and fair coin)
- weighted discrete (biased coin, uses the [alias method](http://www.keithschwarz.com/darts-dice-coins/))
- bernoulli
- geometric
- binomial
- negative binomial
- poisson
- zipf
- uniform continuous
- normal
- cauchy
- chi2
- pareto
- exponential
- lognormal
- student's t-distribution
- gamma
- beta

Methods for manipulating distributions include:

- adding (convolution), subracting (cross-correlation), multiplying and dividing distributions
- joint distributions (a flatMap)
- marginal distributions (a filter)
- creating Markov chains (an iterated flatMap)
- finding the probability of arbitrary predicates, conditional probabililty
- finding expected values (mean), standard deviation, variance, skewness and kurtosis
- sampling, histogram

[Examples.scala](https://github.com/jliszka/probability-monad/blob/master/src/main/scala/probability-monad/Examples.scala) contains some
example uses, and possibly a RISK simulator.

To try out some examples, do

    $ ./sbt console

    scala> runBayesianCoin(5)

Contributions welcome!
