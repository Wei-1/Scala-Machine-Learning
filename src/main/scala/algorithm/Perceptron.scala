// Wei Chen - Perceptron - linear version no kernel
// 2015-11-26

package ght.mi.algorithm
import ght.mi.algorithm.MatrixFunc._

class Perceptron() {
    var projector = Array[(Int, Int, Array[Double], Array[Double])]()
    def clear() = projector = Array[(Int, Int, Array[Double], Array[Double])]()

    private def dot(x:Array[Double], y:Array[Double]): Double =
        arraymultiply(x, y).sum
    // train(Array((1,Array(1,2)),(1,Array(2,3)),(-1,Array(2,1)),(-1,Array(3,2))), 1.5, 100)
    private def dualtrain(
        data: Array[(Int, Array[Double])],
        lambda: Double,
        limit: Int
    ): Array[Double] = {
        val trainingdatasize = data.size
        val featurenumber = data.head._2.size
        var w = Array.fill[Double](featurenumber)(0.0)
        var t = 1
        var cost = -1L
        var i = 0
        while (i < limit){
            for (j <- 0 to trainingdatasize-1){
                val (yi, xi) = data(j)
                if (yi*xi.zip(w).map(l => l._1*l._2).sum < 1) w = w.map(_*(1-1/t)).zip(xi.map(_*yi/lambda/t)).map(l => l._1+l._2)
                else w = w.map(_*(1-1/t))
                t += 1
            }
            val c = data.map(l => 1-l._2.zip(w).map(ll => ll._1 * ll._2).sum).map(l => if (l < 0) 0 else l).sum
            val tmpcost = Math.round((w.map(l => l*l).sum+lambda*c)*10000)
            i += 1
            println("[INFO] "+i+": "+tmpcost)
            if (cost == tmpcost) i = limit
            else cost = tmpcost
        }
        return w
    }

    def train(
        data: Array[(Int, Array[Double])],  // Data Array(yi, xi)
        lambda: Double,                     // Cost of two groups
        limit: Int                          // Iteration limit
    ) = {
        val groupdata = data.groupBy(_._1).map(l => (l._1, l._2.map(_._2)))
        groupdata.map{group1 =>
            groupdata.map{group2 =>
                if (group1._1 < group2._1){
                    val n = group1._2.size + group2._2.size
                    val m = matrixaccumulate(group1._2 ++ group2._2).map(_/n)
                    val w = dualtrain(
                        group1._2.map(g => (-1, arrayminus(g, m))) ++
                            group2._2.map(g => (1, arrayminus(g, m))),
                        lambda,
                        limit
                    )
                    projector :+= (group1._1, group2._1, m, w)
                }
            }
        }
    }

    // --- Dual Predict ---
    def predict(
        data: Array[Array[Double]]
    ): Array[Int] = {
        return data.map{d =>
            projector.map(p =>
                if (dot(arrayminus(d, p._3), p._4) < 0) p._1 else p._2
            ).groupBy(l => l).mapValues(_.size).maxBy(_._2)._1
        }
    }
}

