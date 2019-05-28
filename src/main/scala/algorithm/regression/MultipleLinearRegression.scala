// Wei Chen - Multiple Linear Regression
// 2019-05-27

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class MultipleLinearRegression() extends Regression {
    val algoname: String = "MultipleLinearRegression"
    val version: String = "0.1"

    var slope = Array[Double]()
    var bias = Array[Double]()

    override def clear(): Boolean = try {
        slope = Array[Double]()
        bias = Array[Double]()
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    // --- Start Multiple Linear Regression Function ---
    override def train(
        data: Array[(Double, Array[Double])]   // Data Array(yi, xi)
    ): Boolean = try { // Return PData Class
        val dataSize = data.size
        val xMean = matrixaccumulate(data.map(_._2)).map(_ / dataSize)
        val yMean = data.map(_._1).sum / dataSize
        val xSize = data.head._2.size
        slope = new Array[Double](xSize)
        bias = new Array[Double](xSize)
        val tmpAccu = data.map { case (y, x) =>
            val dy = y - yMean
            val dx = arrayminus(x, xMean)
            (dx.map(_ * dy), dx.map(v => Math.pow(v, 2)))
        }
        val numerator = matrixaccumulate(tmpAccu.map(_._1))
        val denominator = matrixaccumulate(tmpAccu.map(_._2))
        for(i <- 0 until xSize) {
            slope(i) = numerator(i) / denominator(i)
            bias(i) = yMean - slope(i) * xMean(i)
        }
        val loss = new Array[Double](xSize)
        data.map { case (y, x) =>
            for(i <- 0 until xSize) {
                loss(i) += Math.pow((slope(i) * x(i) + bias(i)) - y, 2)
            }
        }
        val lossSum = loss.sum
        val equality = loss.map(l => lossSum - l)
        val equSum = equality.sum
        for(i <- 0 until xSize) {
            slope(i) *= equality(i) / equSum
            bias(i) *= equality(i) / equSum
        }
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }
    // --- Predict Multiple Linear Regression ---
    override def predict(
        data: Array[Array[Double]]
    ): Array[Double] = {
        return data.map { d =>
            val dSize = d.size
            (for(i <- 0 until dSize) yield {
                slope(i) * d(i) + bias(i)
            }).sum
        }
    }
}
