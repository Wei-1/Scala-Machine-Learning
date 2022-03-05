// Wei Chen - Abnormal Trait Test
// 2022-03-05

import com.scalaml.algorithm.Abnormal
import org.scalatest.funsuite.AnyFunSuite

class AbnormalSuite extends AnyFunSuite {

    test("Abnormal Test : Create Sample Algo") {

        class TestAlgo() extends Abnormal {
            val algoname: String = "TestAlgo"
            val version: String = "TestVersion"
            override def clear(): Boolean = true
            override def config(paras: Map[String, Any]): Boolean = true
            override def train(data: Array[Array[Double]]): Boolean = true
            override def predict(data: Array[Array[Double]]): Array[Double] = data.map(_ => 0)
        }

        val ta = new TestAlgo

        assert(ta.algotype == "Abnormal")
        assert(ta.algoname == "TestAlgo")
        assert(ta.version == "TestVersion")
        assert(ta.clear)
        assert(ta.config(Map()))
        assert(ta.train(Array()))
        assert(ta.predict(Array()).size == 0)
        assert(ta.predict(Array(Array(1))).head == 0)
    }
    
}
