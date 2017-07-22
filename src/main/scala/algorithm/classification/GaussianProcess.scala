// Wei Chen - Gaussian Process
// 2016-11-24

package ght.mi.algorithm

import ght.mi.general.MatrixFunc._

class GaussianProcess() {
    var pointGroups = Map[Int, Array[Array[Double]]]()
    def clear() = pointGroups = Map[Int, Array[Array[Double]]]()

    private def prob(a1: Array[Double], a2:Array[Double], s: Double): Double =
        Math.exp(-arrayminussquare(a1, a2).sum / Math.pow(s, 2))
    // --- Start KNN Function ---
    def train(tdata: Array[(Int, Array[Double])]) =
        pointGroups = tdata.groupBy(_._1).map(l => (l._1, l._2.map(_._2)))

    def predict(                            // Gaussian Process
        pdata: Array[Array[Double]],        // Data Array(xi)
        std: Double                         // Standard Deviation
    ): Array[Int] = {                       // Return PData Class
        return pdata.map { pd =>
            pointGroups.map { gdata =>
                (gdata._1, gdata._2.map(gd => prob(pd, gd, std)).sum)
            }.maxBy(_._2)._1            
        }
    }
}
