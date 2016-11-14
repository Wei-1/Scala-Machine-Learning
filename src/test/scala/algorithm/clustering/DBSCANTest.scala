// Wei Chen - DBSCAN Test
// 2016-11-10

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.DBSCAN

class DBSCANSuite extends FunSuite {
    val data = Array(
        Array(1.0, 2.0), Array(1.0, 1.0),
        Array(0.8, 1.0), Array(2.0, 3.0),
        Array(1.1, 1.1), Array(2.0, 2.2),
        Array(6.0, 5.0), Array(6.0, 7.0),
        Array(6.0, 6.6), Array(6.0, 6.1),
        Array(6.0, 6.2), Array(6.0, 7.2)
    )

    val dbscan = new DBSCAN()
    test("DBSCAN Test : Initialization") {
        val result = dbscan.cluster(Array(Array(1.0),Array(-1.0)), 1)
        assert(arrayequal(result.map(_.toDouble), Array(1,2)))
    }

    test("DBSCAN Test : Clustering") {
        val result = dbscan.cluster(data, 2)
        assert(arrayequal(result.map(_.toDouble),
            Array(1,1,1,1,1,1,2,2,2,2,2,2)))
    }
}