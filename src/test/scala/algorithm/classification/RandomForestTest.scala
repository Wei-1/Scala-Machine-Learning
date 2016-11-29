// Wei Chen - Random Forest Test
// 2016-11-29

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.RandomForest

class RandomForestSuite extends FunSuite {
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

    test("RandomForest Test : Initialization") {
        val test = new RandomForest()
        assert(test.trees.isEmpty)
    }

    val rf = new RandomForest()
    test("RandomForest Test : Train") {
        rf.train(traindata, 5, 4)
        assert(!rf.trees.isEmpty)
    }

    test("RandomForest Test : Predict") {
        val result = rf.predict(predictdata)
        assert(arrayequal(result.map(_.toDouble), Array(-1,-1,1,1)))
    }
    
    test("RandomForest Test : Clear") {
        rf.clear()
        assert(rf.trees.isEmpty)
    }
}