// Wei Chen - K-Mean
// 2016-11-07

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class EMCluster() extends Clustering {
    val algoname: String = "EMCluster"
    val version: String = "0.1"

    var gaussians = Array[((Int, Array[Double], Array[Array[Double]]), Int)]()
    var groupdata = Array[(Array[Double], Int)]()
    var k = 2
    var iter = 100

    override def clear(): Boolean = {
        gaussians = Array[((Int, Array[Double], Array[Array[Double]]), Int)]()
        groupdata = Array[(Array[Double], Int)]()
        k = 2
        iter = 100
        true
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        k = paras.getOrElse("K", paras.getOrElse("k", 2)).asInstanceOf[Int]
        iter = paras.getOrElse("ITERATION", paras.getOrElse("iteration", paras.getOrElse("iter", 100))).asInstanceOf[Int]
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }
    // --- Start K-Mean Function ---
    override def cluster( // K Mean
        data: Array[Array[Double]] // Data Array(xi)
    ): Array[Int] = { // Return gaussians
        gaussians = data.zipWithIndex
            .groupBy { l => l._2 % k + 1 }
            .map { l =>
                val c = l._2.size
                val tempdata = l._2.map(_._1)
                val m = matrixaccumulate(tempdata).map(_/c)
                val s = covariance(tempdata)
                ((c, m, s), l._1)
            }.toArray
        var i = 0
        while (i < iter) {
            groupdata = data.map { d =>
                (d, gaussians.map { c =>
                    (c._2, c._1._1 * gaussianprobability(d, c._1._2, c._1._3))
                }.maxBy(_._2)._1)
            }
            val tempgaussians = groupdata.groupBy(_._2).toArray
                .sortBy(_._1).map { l =>
                    val c = l._2.size
                    val tempdata = l._2.map(_._1)
                    val m = matrixaccumulate(tempdata).map(_/c)
                    val s = covariance(tempdata)
                    ((c, m, s), l._1)
                }
            if (gaussians.size == tempgaussians.size &&
                gaussians.zip(tempgaussians).map { l =>
                    if (l._1._2 == l._2._2) arrayequal(l._1._1._2, l._2._1._2)
                    else false
                }.reduce(_ & _)) i = iter
            else gaussians = tempgaussians
        }
        // println(groupdata.map(_._2).mkString(","))
        return groupdata.map(_._2)
    }
}
