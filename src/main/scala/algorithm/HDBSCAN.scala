// Wei Chen - HDBSCAN
// 2016-11-12

package ght.mi.algorithm
import ght.mi.algorithm.MatrixFunc._

// val data = Array(Array(1.0, 2.0), Array(1.0, 1.0), Array(0.8, 1.0),
//     Array(2.0, 3.0), Array(1.1, 1.1), Array(2.0, 2.2), Array(6.0, 5.0),
//     Array(6.0, 7.0), Array(6.0, 6.6), Array(6.0, 6.1), Array(6.0, 6.2))
// val hdbscan = new HDBSCAN()
// hdbscan.cluster(data, 2, 3)

class HDBSCAN() {
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
    // --- HDBSCAN ---
    def cluster(                    // DBSCAN
        data: Array[Array[Double]], // Data Array(xi)
        grouplimit: Int,
        corelimit: Int
    ): Array[Int] = {
        // val corelimit = 3
        val n = data.size;
        val m = data.head.size;
        var distMatrix = Array.fill(n)(Array.fill(n)(0.0))
        for (i <- 0 to n-2) {
            for (j <- i+1 to n-1) {
                distMatrix(i)(j) = distArr(data(i), data(j))
                distMatrix(j)(i) = distArr(data(i), data(j))
            }
        }
        for (i <- 0 to n-1) distMatrix(i)(i) = distMatrix(i).sortBy(l => l).lift(corelimit).get
        for (i <- 0 to n-2) {
            for (j <- i+1 to n-1) {
                val temp = distMatrix(i)(j)
                distMatrix(i)(j) = Math.max(Math.max(distMatrix(i)(i), distMatrix(j)(j)), temp)
                distMatrix(j)(i) = Math.max(Math.max(distMatrix(i)(i), distMatrix(j)(j)), temp)
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
                    val v = distMatrix(node)(i)
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
