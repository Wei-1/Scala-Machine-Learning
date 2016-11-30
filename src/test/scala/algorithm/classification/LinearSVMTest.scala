// Wei Chen - Linear SVM Test
// 2016-06-03

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.LinearSVM

class LinearSVMSuite extends FunSuite {
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
    
    val cost = Map(-1 -> 1.0, 1 -> 1.0)
    val limit = 1000
    val err = 1e-1

    test("LinearSVM Test : Initialization") {
        val test = new LinearSVM()
        assert(test.projector.isEmpty)
    }

    val linearsvm = new LinearSVM()
    test("LinearSVM Test : Train") {
        linearsvm.train(traindata, cost, limit, err)
        assert(!linearsvm.projector.isEmpty)
    }

    test("LinearSVM Test : Predict") {
        val result = linearsvm.predict(predictdata)
        assert(arrayequal(result.map(_.toDouble), Array(-1,-1,1,1)))
    }
    
    test("LinearSVM Test : Clear") {
        linearsvm.clear()
        assert(linearsvm.projector.isEmpty)
    }
}