// Wei Chen - K-Nearest-Neighborhood
// 2015-12-21

package com.interplanetarytech.algorithm

class KNN() extends Classifier {
    var referencepoints = Array[(Int, Array[Double])]()
    var k = 1

    override def clear(): Boolean = try {
        referencepoints = Array[(Int, Array[Double])]()
        k = 1
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def config(paras: Map[String, Double]): Boolean = try {
        k = paras.getOrElse("K", paras.getOrElse("k", 1.0)).toInt
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }
    // --- Start KNN Function ---
    override def train(tdata: Array[(Int, Array[Double])]): Boolean = try {
        referencepoints = tdata
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
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
