// Wei Chen - Algorithm Trait Test
// 2019-07-19

import com.scalaml.algorithm.Algorithm
import org.scalatest.funsuite.AnyFunSuite

class AlgorithmSuite extends AnyFunSuite {

    test("Algorithm Test : Create Sample Sub Trait") {

        trait TestType extends Algorithm {
            val algotype: String = "TestType"
            def testfunc(testinput: Int): Boolean
        }

        class TestAlgo() extends TestType {
            val algoname: String = "TestAlgo"
            val version: String = "TestVersion"
            override def clear(): Boolean = true
            override def config(paras: Map[String, Any]): Boolean = true
            override def testfunc(testinput: Int): Boolean = true
        }

        val ta = new TestAlgo

        assert(ta.algotype == "TestType")
        assert(ta.algoname == "TestAlgo")
        assert(ta.version == "TestVersion")
        assert(ta.clear)
        assert(ta.config(Map()))
        assert(ta.testfunc(0))
    }
    
}
