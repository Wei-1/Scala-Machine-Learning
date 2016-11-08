// Wei Chen - K-Mean
// 2015-12-18

package ght.mi.algorithm
import ght.mi.algorithm.MatrixFunc._

class KMean() {
    var centers = Array[(Array[Double], Int)]()
    var groupdata = Array[(Array[Double], Int)]()
    // --- Start K-Mean Function ---
    def cluster(                  // K Mean
        data: Array[Array[Double]], // Data Array(xi)
        k: Int,                     // K groups
        iter: Int                   // Iteration limit
    ): Array[Int] = {     // Return centers
        centers = data.zipWithIndex
            .groupBy { l => l._2 % k }
            .map { l =>
                val datasize = l._2.size
                matrixaccumulate(l._2.map(_._1)).map(_/datasize)
            }.zipWithIndex.toArray
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
