// Wei Chen - Stochastic Gradient Decent
// 2020-03-08

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class StochasticGradientDecent() extends Regression {
    val algoname: String = "StochasticGradientDecent"
    val version: String = "0.1"

    var weights = Array[Double]()
    var limit = 1000 // for GD
    var batch = 10 // for GD
    var lr = 0.01 // for GD

    override def clear(): Boolean = {
        weights = Array[Double]()
        true
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        limit = paras.getOrElse("LIMIT", paras.getOrElse("limit", 1000)).asInstanceOf[Int]
        batch = paras.getOrElse("BATCH", paras.getOrElse("batch", 10)).asInstanceOf[Int]
        lr = paras.getOrElse("learning_rate", paras.getOrElse("lr", 0.01)).asInstanceOf[Double]
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    // --- Start Stochastic Gradient Decent Function ---
    override def train(
        data: Array[(Double, Array[Double])]   // Data Array(yi, xi)
    ): Boolean = try { // Return PData Class
        val dataSize = data.size
        val y = data.map(_._1)
        val x = data.map(_._2 :+ 1.0)
        val xSize = x.head.size

        for (i <- 0 until limit) {
            val cut1 = (i * batch) % xSize
            val cut2 = cut1 + batch
            weights = gradientDescent(
                x.slice(cut1, cut2),
                y.slice(cut1, cut2),
                lr, 1, weights
            )
        }
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }
    // --- Predict Multivariate Linear Regression ---
    override def predict(
        data: Array[Array[Double]]
    ): Array[Double] = {
        return data.map { d =>
            (d :+ 1.0).zip(weights).map { case (x, w) => w * x }.sum
        }
    }
}
