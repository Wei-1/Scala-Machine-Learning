// Wei Chen - Perceptron - linear version no kernel
// 2015-11-26

package com.interplanetarytech.algorithm
import com.interplanetarytech.general.MatrixFunc._

class Perceptron() {
    var projector = Array[Double]()
    def clear() = projector = Array[Double]()

    private def dot(x:Array[Double], y:Array[Double]): Double =
        arraymultiply(x, y).sum

    // train(Array((1,Array(1,2)),(1,Array(2,3)),(-1,Array(2,1)),(-1,Array(3,2))), 0.5, 100)
    def train(data: Array[(Int, Array[Double])], lr: Double, limit: Int) {
        val traindatasize = data.size
        val featuresize = data.head._2.size
        var w = new Array[Double](featuresize + 1)
        var c = 1.0
        var iter = 0
        var saturated = false
        while (iter < limit && !saturated) {
            iter += 1
            for (j <- 0 until traindatasize) {
                val (yi, xt) = data(j)
                val xi = xt :+ 1.0
                if (yi * dot(xi, w) < 1) w = arraysum(w, xi.map(_ * yi * lr))
            }
            val lostcost = data.map(l => 1 - l._1 * dot(l._2 :+ 1.0, w)).map(Math.max(_, 0)).sum
            val weightcost = w.map(Math.pow(_, 2)).sum
            val tmpcost = weightcost + lostcost
            // println(iter + " " + tmpcost + " " + w.mkString(",") + " " + weightcost + " " + lostcost)
            if (c != tmpcost) c = tmpcost
            else saturated = true
        }
        projector = w
    }

    def predict(data: Array[Array[Double]]): Array[Int] = {
        return data.map { xt =>
            val xi = xt :+ 1.0
            if (dot(xi, projector) < 0) -1
            else 1
        }
    }
}

