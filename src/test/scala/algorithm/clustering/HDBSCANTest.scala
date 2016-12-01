// Wei Chen - HDBSCAN Test
// 2016-11-12

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.HDBSCAN

class HDBSCANSuite extends FunSuite {

    val hdbscan = new HDBSCAN()
    test("HDBSCAN Test : Clustering Tiny Data") {
        val result = hdbscan.cluster(UNLABELED_TINY_DATA, 2, 2)
        assert(arrayequal(result, LABEL_TINY_DATA))
    }

    test("HDBSCAN Test : Clustering Small Data") {
        val result = hdbscan.cluster(UNLABELED_SMALL_DATA, 2, 2)
        assert(arrayequal(result, LABEL_SMALL_DATA))
    }

    test("HDBSCAN Test : Clustering Large Data") {
        val result = hdbscan.cluster(UNLABELED_LARGE_DATA, 2, 2)
        assert(arrayequal(result, LABEL_LARGE_DATA))
    }
}