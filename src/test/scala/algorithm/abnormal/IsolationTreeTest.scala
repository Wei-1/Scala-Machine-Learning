// Wei Chen - Isolation Tree Test
// 2022-03-05

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.IsolationTree
import org.scalatest.funsuite.AnyFunSuite

class IsolationTreeSuite extends AnyFunSuite {

    val itree = new IsolationTree()

    test("IsolationTree Test : Clear") {
        assert(itree.clear())
    }

    test("IsolationTree Test : Abnormal Large Data") {
        assert(itree.clear())
        assert(itree.config(Map[String, Double]()))
        assert(itree.train(UNLABELED_LARGE_DATA))
        val result = itree.predict(UNLABELED_LARGE_DATA)
        assert(arraysimilar(result, UNLABELED_LARGE_DATA.map(_ => 1.0), UNLABELED_NONLINEAR_DATA.size))
    }

    test("IsolationTree Test : Invalid Data") {
        assert(itree.clear())
        assert(!itree.config(Map("maxLayer" -> "test")))
        assert(!itree.train(Array(Array(1, 2), Array())))
    }
}
