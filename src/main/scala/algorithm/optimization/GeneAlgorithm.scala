// Wei Chen - Gene algorithm
// 2017-07-22

package com.interplanetarytech.algorithm

class GeneAlgorithm {
    var seeds: Array[Array[Double]] = null

    def setSeeds(newseeds: Array[Array[Double]]): Unit = seeds = newseeds

    def evolve(
        evaluation: Array[Double] => Double,
        breeding: (Array[Double], Array[Double]) => Array[Array[Double]],
        generationsize: Int = 100,
        elitesize: Int = 3
    ): Array[Double] = {
        val parents = seeds.sortBy { arr =>
            evaluation(arr)
        }.takeRight(elitesize)
        val parentsize = parents.size
        val kids = (0 until generationsize - parentsize).flatMap { i =>
            val ai = scala.util.Random.nextInt.abs % parentsize
            val bi = scala.util.Random.nextInt.abs % parentsize
            breeding(parents(ai), parents(bi))
        }
        seeds = parents ++ kids
        return parents.last // return the best
    }
}
