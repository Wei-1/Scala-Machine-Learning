// Wei Chen - Epsilon Greedy Search
// 2020-03-08

package com.scalaml.algorithm

class EpsilonGreedy {
    var currentScores: Array[Double] = null

    def search(
        evaluation: Array[Double] => Double,
        choices: Array[Array[Double]],
        scores: Array[Double] = null,
        epsilon: Double = 0.1
    ): Array[Double] = {
        val size = choices.size
        if (scores != null)
            currentScores = scores
        if (currentScores == null)
            currentScores = Array.fill[Double](size)(Double.MinValue)
        if (math.random < epsilon) {
            val randSelect = (math.random * size).toInt
            val value = evaluation(choices(randSelect))
            currentScores(randSelect) = value
        }
        choices(currentScores.indexOf(currentScores.max))
    }
}
