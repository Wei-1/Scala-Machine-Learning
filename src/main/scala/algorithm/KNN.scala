// Wei Chen - K-Nearest-Neighborhood
// 2015-12-21

package ght.mi.algorithm

class KNN() {
    var referencepoints = Array[(Int, Array[Double])]()
    def clear() = referencepoints = Array[(Int, Array[Double])]()
    // --- Start KNN Function ---
    def train(tdata: Array[(Int, Array[Double])]) =
        referencepoints = tdata

    def predict(                            // K Mean
        pdata: Array[Array[Double]],        // Data Array(xi)
        k: Int                              // K Nearest
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
