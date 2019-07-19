// Wei Chen - Decision Tree Test
// 2016-11-24

import org.scalatest.FunSuite
import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.DecisionTree

class DecisionTreeSuite extends FunSuite {

    val dtree = new DecisionTree()
    
    test("DecisionTree Test : Clear") {
        assert(dtree.clear())
    }

    test("DecisionTree Test : Linear Data") {
        assert(dtree.clear())
        assert(dtree.config(Map[String, Double]()))
        assert(dtree.train(LABELED_LINEAR_DATA))
        val result = dtree.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))

        val treeString = dtree.tree.toString
        assert(treeString == "col[0] >= 5.0 ? (class[1]) : (class[-1])")
    }

    test("DecisionTree Test : Nonlinear Data - WRONG") {
        assert(dtree.clear())
        assert(dtree.config(Map[String, Double]()))
        assert(dtree.train(LABELED_NONLINEAR_DATA))
        val result = dtree.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }

    test("DecisionTree Test : Invalid Config & Data") {
        assert(dtree.clear())
        assert(!dtree.config(Map("maxLayer" -> "test")))
        assert(!dtree.train(Array((1, Array(1, 2)), (1, Array()))))
    }

}
