// Wei Chen - BIRCH Test
// 2016-06-04

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.BIRCH

class BIRCHSuite extends FunSuite {

    val birch = new BIRCH()
    test("BIRCH Test : Clustering Tiny Data") {
        val result = birch.cluster(UNLABELED_TINY_DATA, 1)
        assert(arrayequal(result, LABEL_TINY_DATA))
    }

    test("BIRCH Test : Clustering Small Data") {
        birch.clear()
        val result = birch.cluster(UNLABELED_SMALL_DATA, 2)
        assert(arrayequal(result, LABEL_SMALL_DATA))
    }

    test("BIRCH Test : Clustering Large Data - WRONG") {
        birch.clear()
        val result = birch.cluster(UNLABELED_LARGE_DATA, 3)
        assert(!arrayequal(result, LABEL_LARGE_DATA))
    }

    test("BIRCH Test : Clearing") {
        birch.clear()
        assert(birch.centers.isEmpty)
    }
}
