// Wei Chen - Linear SVM Test
// 2016-06-03

import org.scalatest.FunSuite
import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.LinearSVM

class LinearSVMSuite extends FunSuite {

    val linearsvm = new LinearSVM()
    test("LinearSVM Test : Clear") {
        assert(linearsvm.clear())
        assert(linearsvm.projector.isEmpty)
    }

    test("LinearSVM Test : Linear Data") {
        assert(linearsvm.clear())
        assert(linearsvm.projector.isEmpty)
        val cost = Map(-1 -> 1.0, 1 -> 1.0)
        val limit = 1000
        val err = 1e-1
        val paras: Map[String, Any] = Map("cost" -> cost, "limit" -> limit, "err" -> err)
        assert(linearsvm.config(paras))
        assert(linearsvm.train(LABELED_LINEAR_DATA))
        assert(!linearsvm.projector.isEmpty)
        val result = linearsvm.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("LinearSVM Test : Nonlinear Data - WRONG") {
        val cost = Map(1 -> 1.0, 2 -> 1.0)
        val limit = 1000
        val err = 1e-1
        val paras: Map[String, Any] = Map("cost" -> cost, "limit" -> limit, "err" -> err)
        assert(linearsvm.clear())
        assert(linearsvm.config(paras))
        assert(linearsvm.train(LABELED_NONLINEAR_DATA))
        assert(!linearsvm.projector.isEmpty)
        val result = linearsvm.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }

    test("LinearSVM Test : Invalid Config & Data") {
        assert(linearsvm.clear())
        assert(!linearsvm.config(Map("limit" -> "test")))
        assert(!linearsvm.train(Array((1, Array(1, 2)), (1, Array(2)), (2, Array(1)), (2, Array()))))
    }
}
