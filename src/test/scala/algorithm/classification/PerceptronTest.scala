// Wei Chen - Perceptron Test
// 2016-06-04

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.Perceptron

class PerceptronSuite extends FunSuite {

    val perceptron = new Perceptron()
    test("Perceptron Test : Initialization") {
        assert(perceptron.projector.isEmpty)
    }

    test("Perceptron Test : Linear Train") {
        val learning_rate = 0.5
        val limit = 1000
        perceptron.train(LABELED_LINEAR_DATA, learning_rate, limit)
        assert(!perceptron.projector.isEmpty)
    }

    test("Perceptron Test : Linear Predict") {
        val result = perceptron.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("Perceptron Test : Nonlinear Train") {
        val learning_rate = 0.5
        val limit = 1000
        perceptron.train(LABELED_NONLINEAR_DATA, learning_rate, limit)
        assert(!perceptron.projector.isEmpty)
    }

    test("Perceptron Test : Nonlinear Predict - WRONG") {
        val result = perceptron.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }
    
    test("Perceptron Test : Clear") {
        perceptron.clear()
        assert(perceptron.projector.isEmpty)
    }
}
