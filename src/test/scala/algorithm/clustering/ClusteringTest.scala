// Wei Chen - Clustering Trait Test
// 2019-07-19

import com.scalaml.algorithm.Clustering
import org.scalatest.funsuite.AnyFunSuite

class ClusteringSuite extends AnyFunSuite {

    test("Clustering Test : Create Sample Algo") {

        class TestAlgo() extends Clustering {
            val algoname: String = "TestAlgo"
            val version: String = "TestVersion"
            override def clear: Boolean = true
            override def config(paras: Map[String, Any]): Boolean = true
            override def cluster(data: Array[Array[Double]]): Array[Int] = data.map(_ => 0)
        }

        val ta = new TestAlgo

        assert(ta.algotype == "Clustering")
        assert(ta.algoname == "TestAlgo")
        assert(ta.version == "TestVersion")
        assert(ta.clear)
        assert(ta.config(Map()))
        assert(ta.cluster(Array()).size == 0)
        assert(ta.cluster(Array(Array(1))).head == 0)
    }
    
}
