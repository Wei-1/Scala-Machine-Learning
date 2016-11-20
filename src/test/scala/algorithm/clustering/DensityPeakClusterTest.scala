// Wei Chen - Density Peak Cluster Test
// 2016-06-03

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.DensityPeakCluster

class DensityPeakClusterSuite extends FunSuite {
    def rint4(x: Double): Double = Math.rint(x * 10000) / 10000
    val data = Array(
        Array(1.0,2.0),
        Array(2.0,2.0),
        Array(1.0,0.1),
        Array(0.0,0.0)
    )

    test("DensityPeakCluster Test : Initialization") {
        val test = new DensityPeakCluster()
        assert(test.dddata.isEmpty)
    }

    val densitypeakcluster = new DensityPeakCluster()
    densitypeakcluster.density(data)
    val dddata = densitypeakcluster.dddata
    test("DensityPeakCluster Test : Density Link") {
        assert(dddata(0)._3 == -1)
    }

    test("DensityPeakCluster Test : Clustering") {
        val result = densitypeakcluster.cluster(0.8, 1.1)
        assert(arrayequal(result.map(_.toDouble), Array(1,1,2,2)))
    }

    test("BayesianDecision Test : Clear") {
        densitypeakcluster.clear()
        val result = densitypeakcluster.dddata
        assert(result.isEmpty)
    }

    test("DensityPeakCluster Test : Import DDData") {
        densitypeakcluster.importdd(dddata)
        val result = densitypeakcluster.dddata
        assert(result(0)._3 == -1)
    }
}