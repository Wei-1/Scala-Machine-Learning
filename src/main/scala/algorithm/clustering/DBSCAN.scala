// Wei Chen - DBSCAN
// 2016-11-10

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

// val data = Array(Array(1.0, 2.0), Array(1.0, 1.0), Array(0.8, 1.0),
//     Array(2.0, 3.0), Array(1.1, 1.1), Array(2.0, 2.2), Array(6.0, 5.0),
//     Array(6.0, 7.0), Array(6.0, 6.6), Array(6.0, 6.1), Array(6.0, 6.2))
// val dbscan = new DBSCAN()
// dbscan.config(Map("limit" -> 2.0))
// dbscan.cluster(data)

class Point(arr: Array[Double], ct: Int) {
    val a = arr
    var c = ct
}

class DBSCAN() extends Clustering {
    val algoname: String = "DBSCAN"
    val version: String = "0.1"

    var groupdata = Array[Point]()
    var limit = 1.0

    override def clear(): Boolean = {
        groupdata = Array[Point]()
        limit = 1.0
        true
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        limit = paras.getOrElse("LIMIT", paras.getOrElse("limit", 1.0)).asInstanceOf[Double]
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    // --- Euclidean Distance ---
    def distPoint(p1: Point, p2: Point): Double =
        Math.sqrt(arrayminussquare(p1.a, p2.a).sum)
    // --- Find all points in a Group ---
    def cascade(p1: Point, c: Int, ind: Int = 0) {
        for (i <- ind until groupdata.size) {
            val p2 = groupdata(i)
            if (p2.c < 0) {
                if (distPoint(p1, p2) < limit) {
                    p2.c = c
                    cascade(p2, c, ind)
                }
            }
        }
    }
    // --- Start DBSCAN Function ---
    override def cluster(                    // DBSCAN
        data: Array[Array[Double]] // Data Array(xi)
    ): Array[Int] = {
        groupdata = data.map(l => new Point(l, -1))
        var c = 1
        for (i <- 0 until groupdata.size) {
            val p1 = groupdata(i)
            if (p1.c < 0) {
                p1.c = c
                cascade(p1, c, i)
                c += 1
            }
        }
        return groupdata.map(_.c)
    }
}
