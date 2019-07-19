// Wei Chen - K-Mean
// 2015-12-18

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class KMean() extends Clustering {
    val algoname: String = "KMean"
    val version: String = "0.1"

    var centers = Array[(Array[Double], Int)]()
    var groupdata = Array[(Array[Double], Int)]()
    var k = 2
    var iter = 100

    override def clear(): Boolean = {
        centers = Array[(Array[Double], Int)]()
        groupdata = Array[(Array[Double], Int)]()
        k = 2
        iter = 100
        true
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        k = paras.getOrElse("K", paras.getOrElse("k", 2)).asInstanceOf[Int]
        iter = paras.getOrElse("ITERATION", paras.getOrElse("iteration", paras.getOrElse("iter", 2))).asInstanceOf[Int]
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }
    // --- Start K-Mean Function ---
    override def cluster(          // K Mean
        data: Array[Array[Double]] // Data Array(xi)
    ): Array[Int] = {     // Return centers
        centers = data.zipWithIndex
            .groupBy { l => l._2 % k + 1 }
            .map { l =>
                val datasize = l._2.size
                (matrixaccumulate(l._2.map(_._1)).map(_/datasize), l._1)
            }.toArray
        var i = 0
        while (i < iter) {
            groupdata = data.map { d =>
                (d, centers.map { c =>
                    (c._2, arrayminussquare(d, c._1).sum)
                }.minBy(_._2)._1)
            }
            val tempcenters = groupdata.groupBy(_._2).map { l =>
                val datasize = l._2.size
                (matrixaccumulate(l._2.map(_._1)).map(_/datasize), l._1)
            }.toArray.sortBy(_._2)
            if (centers.zip(tempcenters).map { l =>
                    if (l._1._2 == l._2._2) arrayequal(l._1._1, l._2._1)
                    else false
                }.reduceLeft(_ && _)) i = iter
            else centers = tempcenters
        }
        return groupdata.map(_._2)
    }
}
