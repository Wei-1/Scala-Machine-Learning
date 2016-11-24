// Wei Chen - Decision Tree Test
// 2016-11-24

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.DecisionTree

class DecisionTreeSuite extends FunSuite {
    val traindata: Array[(Int, Array[Double])] = Array(
        (-1, Array(2,5)),
        (-1, Array(3,4)),
        (-1, Array(4,5)),
        (1, Array(5,4)),
        (1, Array(6,5)),
        (1, Array(7,4)))

    val predictdata: Array[Array[Double]] = Array(
        Array(0,4),
        Array(1,4),
        Array(8,5),
        Array(9,5))

    test("DecisionTree Test : Initialization") {
        val test = new DecisionTree()
        assert(test.tree == null)
    }

    val dtree = new DecisionTree()
    test("DecisionTree Test : Train") {
        dtree.train(traindata)
        assert(dtree.tree != null)
    }

    test("DecisionTree Test : Predict") {
        val result = dtree.predict(predictdata)
        assert(arrayequal(result.map(_.toDouble), Array(-1,-1,1,1)))
    }
    
    test("DecisionTree Test : Clear") {
        dtree.clear()
        assert(dtree.tree == null)
    }
}