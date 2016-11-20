// Wei Chen - Linear SVM Test
// 2016-06-03

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.LinearSVM

class LinearSVMSuite extends FunSuite {
    val traindata = Array(
        Array(-1,2,5),
        Array(-1,3,4),
        Array(-1,4,5),
        Array(1,5,4),
        Array(1,6,5),
        Array(1,7,4)
    ).map(d => (d(0), d.drop(1).map(_.toDouble)))
    val predictdata: Array[Array[Double]] = Array(
        Array(0,4),
        Array(1,4),
        Array(8,5),
        Array(9,5)
    )
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
        assert(linearsvm.projector(0)._1 == -1)
        assert(linearsvm.projector(0)._2 == 1)
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