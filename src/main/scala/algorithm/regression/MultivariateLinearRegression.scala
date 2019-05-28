// Wei Chen - Multivariate Linear Regression - GD
// 2019-05-27
// Use Gradient Decent instead of the optimal solution

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class MultivariateLinearRegression() extends Regression {
    val algoname: String = "MultivariateLinearRegression"
    val version: String = "0.1"

    var weights = Array[Double]()
    var bias = Array[Double]()
    var limit = 1000 // for GD
    var lr = 0.01 // for GD

    override def clear(): Boolean = try {
        weights = Array[Double]()
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        limit = paras.getOrElse("LIMIT", paras.getOrElse("limit", 1000)).asInstanceOf[Int]
        lr = paras.getOrElse("learning_rate", paras.getOrElse("lr", 0.01)).asInstanceOf[Double]
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    // --- Start Multivariate Linear Regression Function ---
    override def train(
        data: Array[(Double, Array[Double])]   // Data Array(yi, xi)
    ): Boolean = try { // Return PData Class
        val dataSize = data.size
        val y = data.map(_._1)
        val x = data.map(_._2 :+ 1.0)
        val xSize = x.head.size

        weights = gradientDescent(x, y, lr, limit)
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
