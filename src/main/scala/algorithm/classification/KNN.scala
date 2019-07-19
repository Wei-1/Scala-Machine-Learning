// Wei Chen - K-Nearest-Neighborhood
// 2015-12-21

package com.scalaml.algorithm

class KNN() extends Classification {
    val algoname: String = "KNN"
    val version: String = "0.1"

    var referencepoints = Array[(Int, Array[Double])]()
    var k = 1

    override def clear(): Boolean = {
        referencepoints = Array[(Int, Array[Double])]()
        k = 1
        true
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        k = paras.getOrElse("K", paras.getOrElse("k", 1.0)).asInstanceOf[Double].toInt
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }
    // --- Start KNN Function ---
    override def train(tdata: Array[(Int, Array[Double])]): Boolean = {
        referencepoints = tdata
        true
    }

    override def predict(                   // K Mean
        pdata: Array[Array[Double]]         // Data Array(xi)
    ): Array[Int] = {                       // Return PData Class
        return pdata.map { pd =>
            referencepoints.map { td =>
                (td._1, td._2.zip(pd).map(l => Math.pow(l._1 - l._2, 2)).sum)
            }.sortBy(_._2)
                .take(k)
                .groupBy(_._1)
                .map(l => (l._2.size, l._2(0)._2, l._1))
                .toArray
                .sortBy(_._2)
                .reverse
                .maxBy(_._1)._3
        }
    }
}
