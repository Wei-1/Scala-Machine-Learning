// Wei Chen - Estimation Maximization Cluster Test
// 2016-06-04

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.general.MatrixFunc._
import ght.mi.algorithm.EMCluster

class EMClusterSuite extends FunSuite {

    val em = new EMCluster()
    test("EMCluster Test : Clustering Tiny Data") {
        val result = em.cluster(UNLABELED_TINY_DATA, 2, 100)
        assert(arrayequal(result, LABEL_TINY_DATA))
    }

    test("EMCluster Test : Clustering Small Data - WRONG") {
        val result = em.cluster(UNLABELED_SMALL_DATA, 2, 100)
        assert(!arrayequal(result, LABEL_SMALL_DATA))
    }

    test("EMCluster Test : Clustering Large Data - WRONG") {
        val result = em.cluster(UNLABELED_LARGE_DATA, 2, 100)
        assert(!arrayequal(result, LABEL_LARGE_DATA))
    }
}