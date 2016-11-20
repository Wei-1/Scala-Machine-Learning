// Wei Chen - DBSCAN
// 2016-11-10

package ght.mi.algorithm
import ght.mi.algorithm.MatrixFunc._

// val data = Array(Array(1.0, 2.0), Array(1.0, 1.0), Array(0.8, 1.0),
//     Array(2.0, 3.0), Array(1.1, 1.1), Array(2.0, 2.2), Array(6.0, 5.0),
//     Array(6.0, 7.0), Array(6.0, 6.6), Array(6.0, 6.1), Array(6.0, 6.2))
// val dbscan = new DBSCAN()
// dbscan.cluster(data, 2)

class Point(arr: Array[Double], ct: Int) {
    val a = arr
    var c = ct
}

class DBSCAN() {
    var groupdata = Array[Point]()
    var distLimit = 0.0
    // --- Euclidean Distance ---
    def distPoint(p1: Point, p2: Point): Double =
        Math.sqrt(arrayminussquare(p1.a, p2.a).sum)
    // --- Find all points in a Group ---
    def cascade(p1: Point, c: Int) {
        for (i <- 0 until groupdata.size) {
            val p2 = groupdata(i)
            if (p2.c < 0) {
                if (distPoint(p1, p2) < distLimit) {
                    groupdata(i).c = c
                    cascade(p2, c)
                }
            }
        }
    }
    // --- Start DBSCAN Function ---
    def cluster(                    // DBSCAN
        data: Array[Array[Double]], // Data Array(xi)
        distlimit: Double
    ): Array[Int] = {
        groupdata = data.map(l => new Point(l, -1))
        distLimit = distlimit
        var c = 1
        for (i <- 0 until groupdata.size) {
            val p1 = groupdata(i)
            if (p1.c < 0) {
                groupdata(i).c = c
                cascade(p1, c)
                c += 1
            }
        }
        return groupdata.map(_.c)
    }
}
