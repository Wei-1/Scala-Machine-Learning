// Wei Chen - Random Cut Tree Test
// 2022-03-05

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.RandomCutTree
import org.scalatest.funsuite.AnyFunSuite

class RandomCutTreeSuite extends AnyFunSuite {

    val rctree = new RandomCutTree()

    test("RandomCutTree Test : Clear") {
        assert(rctree.clear())
    }

    test("RandomCutTree Test : Abnormal Large Data") {
        assert(rctree.clear())
        assert(rctree.config(Map[String, Double]()))
        assert(rctree.train(UNLABELED_LARGE_DATA))
        val result = rctree.predict(UNLABELED_LARGE_DATA)
        assert(arraysimilar(result, UNLABELED_LARGE_DATA.map(_ => 1.0), UNLABELED_NONLINEAR_DATA.size))
    }

    test("RandomCutTree Test : Invalid Data") {
        assert(rctree.clear())
        assert(!rctree.config(Map("maxLayer" -> "test")))
        assert(!rctree.train(Array(Array(1, 2), Array())))
    }
}
