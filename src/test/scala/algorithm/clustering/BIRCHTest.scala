// Wei Chen - BIRCH Test
// 2016-06-04

import org.scalatest.FunSuite
import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.BIRCH

class BIRCHSuite extends FunSuite {

    val birch = new BIRCH()
    test("BIRCH Test : Clustering Tiny Data") {
        assert(birch.clear())
        assert(birch.config(Map("limit" -> 1.0)))
        val result = birch.cluster(UNLABELED_TINY_DATA)
        assert(arrayequal(result, LABEL_TINY_DATA))
    }

    test("BIRCH Test : Clustering Small Data") {
        assert(birch.clear())
        assert(birch.config(Map("limit" -> 2.0)))
        val result = birch.cluster(UNLABELED_SMALL_DATA)
        assert(arrayequal(result, LABEL_SMALL_DATA))
    }

    test("BIRCH Test : Clustering Large Data - WRONG") {
        assert(birch.clear())
        assert(birch.config(Map("limit" -> 3.0)))
        val result = birch.cluster(UNLABELED_LARGE_DATA)
        assert(!arrayequal(result, LABEL_LARGE_DATA))
    }

    test("BIRCH Test : Clearing") {
        assert(birch.clear())
        assert(birch.centers.isEmpty)
    }
}
