// Wei Chen - Upper Confidence Bound Test
// 2020-03-08

import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.UpperConfidenceBound
import org.scalatest.funsuite.AnyFunSuite

class UpperConfidenceBoundSuite extends AnyFunSuite {

    val ucb = new UpperConfidenceBound()


    def evaluation(arr: Array[Double]): Double = 1 / ((arr.head - 0.7).abs + 1)

    val choices: Array[Array[Double]] = Array(
        Array(0.7),
        Array(0.8),
        Array(1.0),
        Array(0.5)
    )
    val c: Double = 1

    test("UpperConfidenceBound Test : Initial") {
        assert(ucb.currentStats == null)
    }

    test("UpperConfidenceBound Test : Search - Start") {
        for (i <- 0 until 100)
            ucb.search(evaluation, choices, null, c)
        assert(ucb.currentStats.size == choices.size)

        val best = ucb.search(evaluation, choices, null, c)
        assert((best.head - 0.7).abs < 0.05)
    }

    test("UpperConfidenceBound Test : Search - Continue") {
        var stats: Array[(Double, Int)] = Array(
            (0, 0),
            (0, 0),
            (1 / 1.3, 1),
            (0, 0)
        )
        for (i <- 0 until 100) {
            ucb.search(evaluation, choices, stats, c)
            stats = ucb.currentStats
        }
        assert(ucb.currentStats.size == stats.size)

        val best = ucb.search(evaluation, choices, stats, c)
        assert((best.head - 0.7).abs < 0.05)
    }

}
