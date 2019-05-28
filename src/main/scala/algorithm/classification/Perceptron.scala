// Wei Chen - Perceptron - linear version no kernel
// 2015-11-26

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class Perceptron() extends Classification {
    val algoname: String = "Perceptron"
    val version: String = "0.1"

    var projector = Array[Double]()
    var lr: Double = 0.5
    var limit: Int = 1000

    override def clear(): Boolean = try {
        projector = Array[Double]()
        lr = 0.5
        limit = 1000
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        lr = paras.getOrElse("LEARNING_RATE", paras.getOrElse("learning_rate", paras.getOrElse("lr", 0.5))).asInstanceOf[Double]
        limit = paras.getOrElse("LIMIT", paras.getOrElse("limit", 1000.0)).asInstanceOf[Double].toInt
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    private def dot(x:Array[Double], y:Array[Double]): Double =
        arraymultiply(x, y).sum

    // train(Array((1,Array(1,2)),(1,Array(2,3)),(-1,Array(2,1)),(-1,Array(3,2))), 0.5, 100)
    override def train(data: Array[(Int, Array[Double])]): Boolean = try {
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
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def predict(data: Array[Array[Double]]): Array[Int] = {
        return data.map { xt =>
            val xi = xt :+ 1.0
            if (dot(xi, projector) < 0) -1
            else 1
        }
    }
}

