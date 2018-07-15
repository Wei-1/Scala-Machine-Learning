// Wei Chen - Random Forest Test
// 2016-11-29

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.RandomForest

class RandomForestSuite extends FunSuite {

    val rf = new RandomForest()
    test("RandomForest Test : Initialization") {
        assert(rf.trees.isEmpty)
    }

    test("RandomForest Test : Linear Train") {
        rf.train(LABELED_LINEAR_DATA, 5, 4)
        assert(!rf.trees.isEmpty)
    }

    test("RandomForest Test : Linear Predict") {
        val result = rf.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("RandomForest Test : Nonlinear Train") {
        rf.train(LABELED_NONLINEAR_DATA, 5, 4)
        assert(!rf.trees.isEmpty)
    }

    test("RandomForest Test : Nonlinear Predict - WRONG") {
        val result = rf.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }
    
    test("RandomForest Test : Clear") {
        rf.clear()
        assert(rf.trees.isEmpty)
    }
}
