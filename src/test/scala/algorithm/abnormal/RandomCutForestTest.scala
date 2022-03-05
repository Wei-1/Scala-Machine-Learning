// Wei Chen - Random Cut Forest Test
// 2022-03-05

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.RandomCutForest
import org.scalatest.funsuite.AnyFunSuite

class RandomCutForestSuite extends AnyFunSuite {

    val rcforest = new RandomCutForest()

    test("RandomCutForest Test : Clear") {
        assert(rcforest.clear())
    }

    test("RandomCutForest Test : Abnormal Large Data") {
        assert(rcforest.clear())
        assert(rcforest.config(Map("tree_n" -> 100.0)))
        assert(rcforest.train(UNLABELED_LARGE_DATA))
        val result = rcforest.predict(UNLABELED_LARGE_DATA)
        assert(arraysimilar(result, UNLABELED_LARGE_DATA.map(_ => 1.0), UNLABELED_NONLINEAR_DATA.size))
        assert(result.last < result.sum / result.size)
    }

    test("RandomCutForest Test : Invalid Data") {
        assert(rcforest.clear())
        assert(!rcforest.config(Map("maxLayer" -> "test")))
        assert(!rcforest.train(Array(Array(1, 2), Array())))
    }
}
