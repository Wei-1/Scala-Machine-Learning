// Wei Chen - Hierarchical Test
// 2016-11-12

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.Hierarchical

class HierarchicalSuite extends FunSuite {

    val hi = new Hierarchical()
    test("Hierarchical Test : Clustering Tiny Data") {
        val result = hi.cluster(UNLABELED_TINY_DATA, 2)
        assert(arrayequal(result, LABEL_TINY_DATA))
    }

    test("Hierarchical Test : Clustering Small Data") {
        val result = hi.cluster(UNLABELED_SMALL_DATA, 2)
        assert(arrayequal(result, LABEL_SMALL_DATA))
    }

    test("Hierarchical Test : Clustering Large Data") {
        val result = hi.cluster(UNLABELED_LARGE_DATA, 2)
        assert(arrayequal(result, LABEL_LARGE_DATA))
    }
}