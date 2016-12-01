// Wei Chen - Linear SVM Test
// 2016-06-03

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.LinearSVM

class LinearSVMSuite extends FunSuite {

    val linearsvm = new LinearSVM()
    test("LinearSVM Test : Initialization") {
        assert(linearsvm.projector.isEmpty)
    }

    test("LinearSVM Test : Linear Train") {
        val cost = Map(-1 -> 1.0, 1 -> 1.0)
        val limit = 1000
        val err = 1e-1
        linearsvm.train(LABELED_LINEAR_DATA, cost, limit, err)
        assert(!linearsvm.projector.isEmpty)
    }

    test("LinearSVM Test : Linear Predict") {
        val result = linearsvm.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }
    
    test("LinearSVM Test : Clear") {
        linearsvm.clear()
        assert(linearsvm.projector.isEmpty)
    }

    test("LinearSVM Test : Nonlinear Train") {
        val cost = Map(1 -> 1.0, 2 -> 1.0)
        val limit = 1000
        val err = 1e-1
        linearsvm.train(LABELED_NONLINEAR_DATA, cost, limit, err)
        assert(!linearsvm.projector.isEmpty)
    }

    test("LinearSVM Test : Nonlinear Predict - WRONG") {
        val result = linearsvm.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }
}