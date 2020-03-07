// Wei Chen - Epsilon Greedy Search Test
// 2020-03-08

import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.EpsilonGreedy
import org.scalatest.funsuite.AnyFunSuite

class EpsilonGreedySuite extends AnyFunSuite {

    val eg = new EpsilonGreedy()


    def evaluation(arr: Array[Double]): Double = 1 / ((arr.head - 0.7).abs + 1)

    val choices: Array[Array[Double]] = Array(
        Array(0.7),
        Array(0.8),
        Array(1.0),
        Array(0.5)
    )
    val epsilon: Double = 0.1

    test("GeneAlgorithm Test : Initial") {
        assert(eg.currentScores == null)
    }

    test("GeneAlgorithm Test : Search - Start") {
        for (i <- 0 until 1000)
            eg.search(evaluation, choices, null, epsilon)
        assert(eg.currentScores.size == choices.size)

        val best = eg.search(evaluation, choices, null, epsilon)
        assert((best.head - 0.7).abs < 0.05)
    }

    test("GeneAlgorithm Test : Search - Continue") {
        var scores: Array[Double] = Array(0, 0, 1 / 1.3, 0)
        for (i <- 0 until 1000) {
            eg.search(evaluation, choices, scores, epsilon)
            scores = eg.currentScores
        }
        assert(eg.currentScores.size == scores.size)

        val best = eg.search(evaluation, choices, scores, epsilon)
        assert((best.head - 0.7).abs < 0.05)
    }

}
