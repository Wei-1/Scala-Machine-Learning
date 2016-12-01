// Wei Chen - Density Peak Cluster Test
// 2016-06-03

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.DensityPeakCluster

class DensityPeakClusterSuite extends FunSuite {

    val dpc = new DensityPeakCluster()
    test("DensityPeakCluster Test : Clustering Tiny Data") {
        dpc.density(UNLABELED_TINY_DATA, 1)
        val result = dpc.cluster(1, 1)
        assert(arrayequal(result, LABEL_TINY_DATA.reverse))
    }

    test("DensityPeakCluster Test : Clustering Small Data") {
        dpc.density(UNLABELED_SMALL_DATA, 2)
        val result = dpc.cluster(1, 2)
        assert(arrayequal(result, LABEL_SMALL_DATA.reverse))
    }

    test("DensityPeakCluster Test : Clustering Large Data") {
        dpc.density(UNLABELED_LARGE_DATA, 3)
        val result = dpc.cluster(12, 2)
        assert(arrayequal(result, LABEL_LARGE_DATA))
    }

    test("DensityPeakCluster Test : Clear") {
        dpc.clear()
        assert(dpc.dddata.isEmpty)
    }
}