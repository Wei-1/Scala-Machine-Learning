// Wei Chen - Regression Trait Test
// 2019-07-19

import com.scalaml.algorithm.Regression
import org.scalatest.funsuite.AnyFunSuite

class RegressionSuite extends AnyFunSuite {

    test("Regression Test : Create Sample Algo") {

        class TestAlgo() extends Regression {
            val algoname: String = "TestAlgo"
            val version: String = "TestVersion"
            override def clear(): Boolean = true
            override def config(paras: Map[String, Any]): Boolean = true
            override def train(data: Array[(Double, Array[Double])]): Boolean = true
            override def predict(data: Array[Array[Double]]): Array[Double] = data.map(_ => 0.0)
        }

        val ta = new TestAlgo

        assert(ta.algotype == "Regression")
        assert(ta.algoname == "TestAlgo")
        assert(ta.version == "TestVersion")
        assert(ta.clear)
        assert(ta.config(Map()))
        assert(ta.train(Array()))
        assert(ta.predict(Array()).size == 0)
        assert(ta.predict(Array(Array(1.0))).head == 0.0)
    }
    
}
