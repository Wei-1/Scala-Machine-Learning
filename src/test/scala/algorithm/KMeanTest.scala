// Wei Chen - K Mean Cluster Test
// 2016-06-04

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.KMean

class KMeanSuite extends FunSuite {
    val data = Array(
        Array(1.0,2.0),
        Array(2.0,2.0),
        Array(1.0,0.1),
        Array(0.0,0.0)
    )

    val kmean = new KMean()
    test("KMean Test : Initialization"){
        val result = kmean.cluster(Array(Array(1.0),Array(-1.0)), 2, 100)
        assert(arrayequal(result.map(_.toDouble), Array(1,0)))
    }

    test("KMean Test : Clustering"){
        val result = kmean.cluster(data, 2, 100)
        assert(arrayequal(result.map(_.toDouble), Array(1,1,0,0)))
    }
}