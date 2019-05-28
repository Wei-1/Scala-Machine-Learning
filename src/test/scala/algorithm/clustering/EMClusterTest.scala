// Wei Chen - Estimation Maximization Cluster Test
// 2016-06-04

import org.scalatest.FunSuite
import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.EMCluster

class EMClusterSuite extends FunSuite {

    val em = new EMCluster()
    test("EMCluster Test : Clustering Tiny Data") {
        assert(em.clear())
        assert(em.config(Map("k" -> 2, "iter" -> 100)))
        val result = em.cluster(UNLABELED_TINY_DATA)
        assert(arrayequal(result, LABEL_TINY_DATA))
    }

    test("EMCluster Test : Clustering Small Data - WRONG") {
        assert(em.clear())
        assert(em.config(Map("k" -> 2, "iter" -> 100)))
        val result = em.cluster(UNLABELED_SMALL_DATA)
        assert(!arrayequal(result, LABEL_SMALL_DATA))
    }

    test("EMCluster Test : Clustering Large Data - WRONG") {
        assert(em.clear())
        assert(em.config(Map("k" -> 2, "iter" -> 100)))
        val result = em.cluster(UNLABELED_LARGE_DATA)
        assert(!arrayequal(result, LABEL_LARGE_DATA))
    }
}
