package probability_monad.test

import probability_monad._

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec

import org.scalactic._
import TripleEquals._
 import TolerantNumerics._

class DistributionSpec extends AnyFreeSpec {

  implicit val dblEquality = tolerantDoubleEquality(0.1)

  "Expected Value" in {
    assert(Distribution.uniform.ev === 0.5)
    assert(Distribution.normal.ev === 0.0)
    assert(Distribution.lognormal.ev === 1.6)
  }
}
