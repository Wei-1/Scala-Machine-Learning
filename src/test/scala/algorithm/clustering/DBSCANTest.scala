// Wei Chen - DBSCAN Test
// 2016-11-10

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.DBSCAN

class DBSCANSuite extends FunSuite {

    val dbscan = new DBSCAN()
    test("DBSCAN Test : Clustering Tiny Data") {
        val result = dbscan.cluster(UNLABELED_TINY_DATA, 2)
        assert(arrayequal(result, LABEL_TINY_DATA))
    }

    test("DBSCAN Test : Clustering Small Data") {
        val result = dbscan.cluster(UNLABELED_SMALL_DATA, 2)
        assert(arrayequal(result, LABEL_SMALL_DATA))
    }

    test("DBSCAN Test : Clustering Large Data") {
        val result = dbscan.cluster(UNLABELED_LARGE_DATA, 3)
        assert(arrayequal(result, LABEL_LARGE_DATA))
    }
}
