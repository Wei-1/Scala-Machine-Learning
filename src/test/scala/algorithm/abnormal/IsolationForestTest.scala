// Wei Chen - Isolation Forest Test
// 2022-03-05

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.IsolationForest
import org.scalatest.funsuite.AnyFunSuite

class IsolationForestSuite extends AnyFunSuite {

    val iforest = new IsolationForest()

    test("IsolationForest Test : Clear") {
        assert(iforest.clear())
    }

    test("IsolationForest Test : Abnormal Large Data") {
        assert(iforest.clear())
        assert(iforest.config(Map("tree_n" -> 100.0)))
        assert(iforest.train(UNLABELED_LARGE_DATA))
        val result = iforest.predict(UNLABELED_LARGE_DATA)
        assert(arraysimilar(result, UNLABELED_LARGE_DATA.map(_ => 1.0), UNLABELED_NONLINEAR_DATA.size))
        assert(result.last < result.sum / result.size)
    }

    test("IsolationForest Test : Invalid Data") {
        assert(iforest.clear())
        assert(!iforest.config(Map("maxLayer" -> "test")))
        assert(!iforest.train(Array(Array(1, 2), Array())))
    }
}
