// Wei Chen - Perceptron Test
// 2016-06-04

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.Perceptron
import org.scalatest.funsuite.AnyFunSuite

class PerceptronSuite extends AnyFunSuite {

    val perceptron = new Perceptron()

    test("Perceptron Test : Clear") {
        assert(perceptron.clear())
    }

    test("Perceptron Test : Linear Data") {
        assert(perceptron.clear())
        assert(perceptron.config(Map("learning_rate" -> 0.5, "limit" -> 1000.0)))
        assert(perceptron.train(LABELED_LINEAR_DATA))
        val result = perceptron.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("Perceptron Test : Nonlinear Data - WRONG") {
        assert(perceptron.clear())
        assert(perceptron.config(Map("learning_rate" -> 0.5, "limit" -> 1000.0)))
        assert(perceptron.train(LABELED_NONLINEAR_DATA))
        val result = perceptron.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }

    test("Perceptron Test : Invalid Config") {
        assert(perceptron.clear())
        assert(!perceptron.config(Map("limit" -> "test")))
    }
}
