// Wei Chen - Upper Confidence Bound
// 2020-03-08

package com.scalaml.algorithm

class UpperConfidenceBound {
    var currentStats: Array[(Double, Int)] = null

    def select(c: Double): Int = {
        val n = currentStats.count(_._2 > 0)
        val currentScores = currentStats.map { case (m, kn) =>
            m + c * math.sqrt(math.log(n + 1) / (kn + 1e-12))
        }
        currentScores.indexOf(currentScores.max)
    }

    def add(i: Int, value: Double) {
        val (currentValue, currentCount) = currentStats(i)
        val newValue = (currentValue * currentCount + value) / (currentCount + 1)
        currentStats(i) = (newValue, currentCount + 1)
    }

    def search(
        evaluation: Array[Double] => Double,
        choices: Array[Array[Double]],
        scores: Array[(Double, Int)] = null,
        c: Double = 1
    ): Array[Double] = {
        val size = choices.size
        if (scores != null)
            currentStats = scores
        if (currentStats == null)
            currentStats = Array.fill[(Double, Int)](size)((0, 0))
        val currentSelect = select(c)
        val value = evaluation(choices(currentSelect))
        add(currentSelect, value)
        choices(currentStats.indexOf(currentStats.maxBy(_._1)))
    }
}
