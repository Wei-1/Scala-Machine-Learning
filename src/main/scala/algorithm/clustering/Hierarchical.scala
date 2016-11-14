// Wei Chen - HDBSCAN
// 2016-11-12

package ght.mi.algorithm
import ght.mi.algorithm.MatrixFunc._

// val data = Array(Array(1.0, 2.0), Array(1.0, 1.0), Array(0.8, 1.0),
//     Array(2.0, 3.0), Array(1.1, 1.1), Array(2.0, 2.2), Array(6.0, 5.0),
//     Array(6.0, 7.0), Array(6.0, 6.6), Array(6.0, 6.1), Array(6.0, 6.2))
// val hdbscan = new HDBSCAN()
// hdbscan.cluster(data, 2, 3)

class Hierarchical() {
    var splittree = Array[(Int, Int)]()
    var treecheck = Map[Int, Int]()
    def distArr(a1: Array[Double], a2: Array[Double]): Double =
        Math.sqrt(arrayminussquare(a1, a2).sum)
    def cascade(p: Int, c: Int) {
        splittree.map{ l =>
            if (l._1 == p && treecheck(l._2) < 0) {
                treecheck += (l._2 -> c)
                cascade(l._2, c)
            } else if (l._2 == p && treecheck(l._1) < 0) {
                treecheck += (l._1 -> c)
                cascade(l._1, c)
            }
        }
    }
    // --- Hierarchical ---
    def cluster(                    // Hierarchical
        data: Array[Array[Double]], // Data Array(xi)
        grouplimit: Int
    ): Array[Int] = {
        val n = data.size;
        val m = data.head.size;
        var distMatrix3 = new Array[Double](n*(n-1)/2)
        def setM3(x: Int, y: Int, v: Double) {
            if (x < y) distMatrix3(x*n + y - ((Math.pow(x, 2) + 3*x)/2).toInt - 1) = v
            else if (y < x) distMatrix3(y*n + x - ((Math.pow(y, 2) + 3*y)/2).toInt - 1) = v
        }
        def getM3xy(x: Int, y: Int): Double = {
            if (x < y) return distMatrix3(x*n + y - ((Math.pow(x, 2) + 3*x)/2).toInt - 1)
            else if (y < x) return distMatrix3(y*n + x - ((Math.pow(y, 2) + 3*y)/2).toInt - 1)
            else return 0.0
        }
        
        for (i <- 0 to n-2) {
            for (j <- i+1 to n-1) {
                setM3(i, j, distArr(data(i), data(j)))
            }
        }
        var undone = Map(0 -> 0.0)
        undone ++= (1 to n-1).map((_, Double.MaxValue))
        var tree = Map[Int, (Int, Double)]()
        while (!undone.isEmpty) {
            val node = undone.minBy(_._2)._1
            undone -= node
            for (i <- 0 to n-1) {
                if (i != node) {
                    val v = getM3xy(node, i)
                    if (undone.contains(i) && v < undone(i)) {
                        undone += (i -> v)
                        tree += (i -> (node, v))
                    }
                }
            }
        }
        splittree = tree.toArray.sortBy(_._2._2).dropRight(grouplimit-1).map(l => (l._1, l._2._1))
        treecheck = (0 to n-1).map((_, -1)).toMap
        var c = 1
        for (i <- 0 to n-1) {
            if (treecheck(i) < 0) {
                treecheck += (i -> c)
                cascade(i, c)
                c += 1
            }
        }
        return treecheck.toArray.sortBy(_._1).map(_._2)
    }
}
