// Wei Chen - BIRCH
// 2016-11-18

package ght.mi.algorithm
import ght.mi.algorithm.MatrixFunc._

class BIRCH() {
    var centers = Array[(Int, Array[Double], Array[Double])]()
    // --- Start BIRCH Function ---
    def cluster(                    // BIRCH
        data: Array[Array[Double]], // Data Array(xi)
        limit: Double               // Group Separation Length
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
                        val v = l._2 - Math.pow(l._1, 2) / c._1
                        if (v < 1e-20) 1e-20
                        else v
                    }
                    val r = Math.sqrt(1 / xyz2.zip(varr).map(l => l._1 / l._2).sum)
                    val temp_len = Math.sqrt(xyz2.sum) * (1 - r)
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
    // --- Clear Centers ---
    def clear() {
        centers = Array[(Int, Array[Double], Array[Double])]()
    }
}
