# Simple Probability Monad

Makes it easy to create and manipulate probability distributions and sample them.

For example, here's how you would code up the following problem: You are given either a fair coin or a
biased coin with equal probability. If you flip it 5 times and it comes up heads each time, what is the
probability you have the fair coin?

    def bayesianCoin(flips: Int) = {
      for {
        whichCoin <- d(2)
        c = if (whichCoin == 1) coin else biasedCoin(0.9)
        results <- c.repeat(flips)
      } yield (whichCoin, results)
    }
  
    bayesianCoin(5).given(_._2.forall(_ == H)).p(_._1 == 1)

[Distribution.scala](https://github.com/jliszka/probability-monad/blob/master/Distribution.scala) contains code
for creating and manipulating probability distributions.

[Examples.scala](https://github.com/jliszka/probability-monad/blob/master/Examples.scala) contains some 
examples, possibly a RISK simulator.