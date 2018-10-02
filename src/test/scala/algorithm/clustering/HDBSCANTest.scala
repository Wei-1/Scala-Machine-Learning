// Wei Chen - HDBSCAN Test
// 2016-11-12

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.HDBSCAN

class HDBSCANSuite extends FunSuite {

    val hdbscan = new HDBSCAN()
    test("HDBSCAN Test : Clustering Tiny Data") {
        assert(hdbscan.clear())
        assert(hdbscan.config(Map("k" -> 2, "limit" -> 2)))
        val result = hdbscan.cluster(UNLABELED_TINY_DATA)
        assert(arrayequal(result, LABEL_TINY_DATA))
    }

    test("HDBSCAN Test : Clustering Small Data") {
        assert(hdbscan.clear())
        assert(hdbscan.config(Map("k" -> 2, "limit" -> 2)))
        val result = hdbscan.cluster(UNLABELED_SMALL_DATA)
        assert(arrayequal(result, LABEL_SMALL_DATA))
    }

    test("HDBSCAN Test : Clustering Large Data") {
        assert(hdbscan.clear())
        assert(hdbscan.config(Map("k" -> 2, "limit" -> 2)))
        val result = hdbscan.cluster(UNLABELED_LARGE_DATA)
        assert(arrayequal(result, LABEL_LARGE_DATA))
    }
}
