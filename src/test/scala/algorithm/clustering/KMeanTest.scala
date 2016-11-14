// Wei Chen - K Mean Cluster Test
// 2016-06-04

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.KMean

class KMeanSuite extends FunSuite {
    val data = Array(Array(1.0, 2.0), Array(1.0, 1.0), Array(0.8, 1.0),
    Array(2.0, 3.0), Array(1.1, 1.1), Array(2.0, 2.2), Array(6.0, 5.0),
    Array(6.0, 7.0), Array(6.0, 6.6), Array(6.0, 6.1), Array(6.0, 6.2))

    val kmean = new KMean()
    test("KMean Test : Initialization") {
        val result = kmean.cluster(Array(Array(1.0),Array(-1.0)), 2, 100)
        assert(arrayequal(result.map(_.toDouble), Array(1,2)))
    }

    test("KMean Test : Clustering") {
        val result = kmean.cluster(data, 2, 100)
        assert(arrayequal(result.map(_.toDouble),
            Array(1,1,1,1,1,1,2,2,2,2,2,2)))    }
}