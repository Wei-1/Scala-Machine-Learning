// Wei Chen - Random Forest Test
// 2016-11-29

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.RandomForest
import org.scalatest.funsuite.AnyFunSuite

class RandomForestSuite extends AnyFunSuite {

    val rf = new RandomForest()

    test("RandomForest Test : Clear") {
        assert(rf.clear())
    }

    test("RandomForest Test : Linear Data") {
        assert(rf.clear())
        assert(rf.config(Map("tree_n" -> 5.0, "sample_n" -> 4.0)))
        assert(rf.train(LABELED_LINEAR_DATA))
        val result = rf.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }
    
    test("RandomForest Test : Nonlinear Data - WRONG") {
        assert(rf.clear())
        assert(rf.config(Map("tree_n" -> 20.0, "sample_n" -> 4.0)))
        assert(rf.train(LABELED_NONLINEAR_DATA))
        val result = rf.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }

    test("RandomForest Test : Invalid Config & Data") {
        assert(rf.clear())
        assert(!rf.config(Map("maxLayer" -> "test")))
        assert(rf.config(Map("tree_n" -> 5.0, "sample_n" -> 4.0)))
        assert(!rf.train(Array((1, Array(1, 2)), (1, Array(2)), (2, Array()), (2, Array(1)))))
    }
}
