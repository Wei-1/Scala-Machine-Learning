// Wei Chen - Gradient Boost
// 2022-12-27

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class GradientBoost() extends Regression {
    val algoname: String = "GradientBoost"
    val version: String = "0.1"

    var regressors = Array[Regression]()

    override def clear(): Boolean = {
        regressors = Array[Regression]()
        true
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        regressors = paras.getOrElse("REGRESSORS", paras.getOrElse("regressors", Array(new StochasticGradientDecent): Any)).asInstanceOf[Array[Regression]]
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def train(data: Array[(Double, Array[Double])]): Boolean = {
        var check = regressors.size > 0
        var residue = Array.fill(data.size)(0.0)
        for (regressor <- regressors) {
            val tmpdata = data.zip(residue).map { case (d, r) => (d._1 + r, d._2) }
            check &= regressor.train(tmpdata)
            residue = arrayminus(data.map(_._1), regressor.predict(data.map(_._2)))
        }
        check
    }

    override def predict(data: Array[Array[Double]]): Array[Double] = {
        val results = regressors.map(regressor => regressor.predict(data))
        matrixaccumulate(results).map(_ / regressors.size)
    }
}
