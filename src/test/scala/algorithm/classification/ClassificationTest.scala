// Wei Chen - Classification Trait Test
// 2019-07-19

import org.scalatest.FunSuite
import com.scalaml.algorithm.Classification

class ClassificationSuite extends FunSuite {

    test("Classification Test : Create Sample Algo") {

        class TestAlgo() extends Classification {
            val algoname: String = "TestAlgo"
            val version: String = "TestVersion"
            override def clear: Boolean = true
            override def config(paras: Map[String, Any]): Boolean = true
            override def train(data: Array[(Int, Array[Double])]): Boolean = true
            override def predict(data: Array[Array[Double]]): Array[Int] = data.map(_ => 0)
        }

        val ta = new TestAlgo

        assert(ta.algotype == "Classification")
        assert(ta.algoname == "TestAlgo")
        assert(ta.version == "TestVersion")
        assert(ta.clear)
        assert(ta.config(Map()))
        assert(ta.train(Array()))
        assert(ta.predict(Array()).size == 0)
        assert(ta.predict(Array(Array(1))).head == 0)
    }
    
}
