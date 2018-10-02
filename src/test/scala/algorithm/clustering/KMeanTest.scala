// Wei Chen - K Mean Cluster Test
// 2016-06-04

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.KMean

class KMeanSuite extends FunSuite {

    val km = new KMean()
    test("KMean Test : Clustering Tiny Data") {
        assert(km.clear())
        assert(km.config(Map("k" -> 2, "iter" -> 100)))
        val result = km.cluster(UNLABELED_TINY_DATA)
        assert(arrayequal(result, LABEL_TINY_DATA))
    }

    test("KMean Test : Clustering Small Data") {
        assert(km.clear())
        assert(km.config(Map("k" -> 2, "iter" -> 100)))
        val result = km.cluster(UNLABELED_SMALL_DATA)
        assert(arrayequal(result, LABEL_SMALL_DATA))
    }

    test("KMean Test : Clustering Large Data - WRONG") {
        assert(km.clear())
        assert(km.config(Map("k" -> 2, "iter" -> 100)))
        val result = km.cluster(UNLABELED_LARGE_DATA)
        assert(!arrayequal(result, LABEL_LARGE_DATA))
    }
}
