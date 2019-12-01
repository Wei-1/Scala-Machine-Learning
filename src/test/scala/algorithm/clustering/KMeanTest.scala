// Wei Chen - K Mean Cluster Test
// 2016-06-04

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.KMean
import org.scalatest.funsuite.AnyFunSuite

class KMeanSuite extends AnyFunSuite {

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

    test("KMean Test : Invalid Config") {
        assert(km.clear())
        assert(!km.config(Map("k" -> "test")))
    }
}
