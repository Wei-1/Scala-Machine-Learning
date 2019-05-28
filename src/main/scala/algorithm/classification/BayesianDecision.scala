// Wei Chen - Bayesian Decision
// 2015-12-20

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class BayesianDecision() extends Classification {
    val algoname: String = "BayesianDecision"
    val version: String = "0.1"

    var groupcnt = Map[Int, Int]()
    var groupavg = Map[Int, Array[Double]]()
    var groupcov = Map[Int, Array[Array[Double]]]()

    override def clear(): Boolean = try {
        groupcnt = Map[Int, Int]()
        groupavg = Map[Int, Array[Double]]()
        groupcov = Map[Int, Array[Array[Double]]]()
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

    private def arrstats(
        i: Int,
        data: Array[Array[Double]]
    ) {
        val n = data.size
        val m = matrixaccumulate(data).map(_/n)
        val s = covariance(data)
        groupcnt += (i -> n)
        groupavg += (i -> m)
        groupcov += (i -> s)
    }

    override def train(
        data: Array[(Int, Array[Double])]
    ): Boolean = try {
        data.groupBy(_._1).foreach { d =>
            arrstats(d._1, d._2.map(_._2))
        }
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def predict(
        data: Array[Array[Double]]
    ): Array[Int] = {
        val n = groupavg.size
        if (n == 0) {
            Array[Int]()
        } else {
            data.map { d =>
                groupcnt.map { cnt =>
                    val i = cnt._1
                    (i, cnt._2 * gaussianprobability(d, groupavg(i), groupcov(i)))
                }.toArray.maxBy(_._2)._1
            }
        }
    }
}
