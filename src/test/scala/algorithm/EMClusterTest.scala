// Wei Chen - Estimation Maximization Cluster Test
// 2016-06-04

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.EMCluster

class EMClusterSuite extends FunSuite {
    val data = Array(
        Array(1.0,2.0),
        Array(2.0,2.0),
        Array(1.0,0.1),
        Array(0.0,0.0)
    )

    val em = new EMCluster()
    test("EMCluster Test : Initialization"){
        val result = em.cluster(Array(Array(1.0),Array(-1.0)), 2, 100)
        assert(arrayequal(result.map(_.toDouble), Array(1,0)))
    }

    test("EMCluster Test : Clustering"){
        val result = em.cluster(data, 2, 100)
        assert(arrayequal(result.map(_.toDouble), Array(1,0,1,0)))
    }
}