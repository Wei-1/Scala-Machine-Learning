// Wei Chen - BIRCH
// 2016-11-18

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class BIRCH() extends Clustering {
    val algoname: String = "BIRCH"
    val version: String = "0.1"

    var centers = Array[(Int, Array[Double], Array[Double])]()
    var limit = 1.0

    override def clear(): Boolean = try {
        centers = Array[(Int, Array[Double], Array[Double])]()
        limit = 1.0
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        limit = paras.getOrElse("LIMIT", paras.getOrElse("limit", 1.0)).asInstanceOf[Double]
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    def notzero(x: Double): Double =
        if (x >= 0 && x < 1e-20) 1e-20
        else if (x < 0 && x > -1e-20) -1e-20
        else x
    // --- Start BIRCH Function ---
    override def cluster(                    // BIRCH
        data: Array[Array[Double]] // Data Array(xi)
    ): Array[Int] = { // Return Centers
        return data.map { d =>
            val ss = d.map(Math.pow(_, 2))
            var g = -1
            if (centers.size == 0) {
                centers :+= (1, d, ss)
                1
            } else {
                var i = -1
                var len = limit
                for (ci <- 0 until centers.size) {
                    val c = centers(ci)
                    val m = c._2.map(_ / c._1)
                    val xyz2 = arrayminussquare(m, d)
                    val varr = c._2.zip(c._3).map { l =>
                        notzero(l._2 - Math.pow(l._1, 2) / c._1)
                    }
                    val r = Math.sqrt(1 / notzero(xyz2.zip(varr).map(l => l._1 / l._2).sum))
                    val temp_len = Math.sqrt(xyz2.sum) * (1 - r)
                    // println(d.mkString(",") + "  " + varr.mkString(",") + "  " + temp_len)
                    if (temp_len < len) {
                        i = ci
                        len = temp_len
                    }
                }
                if (i == -1) {
                    centers :+= (1, d, ss)
                    centers.size
                } else {
                    val c = centers(i)
                    centers(i) = (c._1 + 1, arraysum(c._2, d), arraysum(c._3, ss))
                    i + 1
                }
            }
        }
    }
}
