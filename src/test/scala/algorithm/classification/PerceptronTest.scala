// Wei Chen - Perceptron Test
// 2016-06-04

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.Perceptron

class PerceptronSuite extends FunSuite {

    val perceptron = new Perceptron()

    test("Perceptron Test : Clear") {
        assert(perceptron.clear())
    }

    test("Perceptron Test : Linear Data") {
        assert(perceptron.clear())
        assert(perceptron.config(Map("learning_rate" -> 0.5, "limit" -> 1000)))
        assert(perceptron.train(LABELED_LINEAR_DATA))
        val result = perceptron.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("Perceptron Test : Nonlinear Data - WRONG") {
        assert(perceptron.clear())
        assert(perceptron.config(Map("learning_rate" -> 0.5, "limit" -> 1000)))
        assert(perceptron.train(LABELED_NONLINEAR_DATA))
        val result = perceptron.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }
}
