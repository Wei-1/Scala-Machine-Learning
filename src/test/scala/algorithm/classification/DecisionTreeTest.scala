// Wei Chen - Decision Tree Test
// 2016-11-24

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.DecisionTree

class DecisionTreeSuite extends FunSuite {

    val dtree = new DecisionTree()
    
    test("DecisionTree Test : Clear") {
        assert(dtree.clear())
    }

    test("DecisionTree Test : Linear Data") {
        assert(dtree.clear())
        assert(dtree.confid(Map[String, Double]()))
        assert(dtree.train(LABELED_LINEAR_DATA))
        val result = dtree.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("DecisionTree Test : Nonlinear Data - WRONG") {
        assert(dtree.clear())
        assert(dtree.confid(Map[String, Double]()))
        assert(dtree.train(LABELED_NONLINEAR_DATA))
        val result = dtree.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }
}
