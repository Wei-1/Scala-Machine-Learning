// Wei Chen - Random Forest Test
// 2016-11-29

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.RandomForest

class RandomForestSuite extends FunSuite {

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
    
    test("RandomForest Test : Nonlinear Data") {
        assert(rf.clear())
        assert(rf.config(Map("tree_n" -> 20.0, "sample_n" -> 4.0)))
        assert(rf.train(LABELED_NONLINEAR_DATA))
        val result = rf.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }
}
