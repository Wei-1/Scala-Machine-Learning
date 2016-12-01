// Wei Chen - Decision Tree Test
// 2016-11-24

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.DecisionTree

class DecisionTreeSuite extends FunSuite {

    val dtree = new DecisionTree()
    test("DecisionTree Test : Initialization") {
        assert(dtree.tree == null)
    }

    test("DecisionTree Test : Linear Train") {
        dtree.train(LABELED_LINEAR_DATA)
        assert(dtree.tree != null)
    }

    test("DecisionTree Test : Linear Predict") {
        val result = dtree.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("DecisionTree Test : Nonlinear Train") {
        dtree.train(LABELED_NONLINEAR_DATA)
        assert(dtree.tree != null)
    }

    test("DecisionTree Test : Nonlinear Predict - WRONG") {
        val result = dtree.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }
    
    test("DecisionTree Test : Clear") {
        dtree.clear()
        assert(dtree.tree == null)
    }
}