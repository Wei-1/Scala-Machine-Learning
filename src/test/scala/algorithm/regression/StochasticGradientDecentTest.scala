// Wei Chen - Multivariate Linear Regression Test
// 2016-06-04

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.StochasticGradientDecent
import org.scalatest.funsuite.AnyFunSuite

class StochasticGradientDecentSuite extends AnyFunSuite {

    val sgd = new StochasticGradientDecent()
    
    test("StochasticGradientDecent Test : Clear") {
        assert(sgd.clear())
    }

    test("StochasticGradientDecent Test : Linear Data") {
        assert(sgd.clear())
        assert(sgd.config(Map[String, Double]()))
        assert(sgd.train(LABELED_LINEAR_DATA.map(yx => yx._1.toDouble -> yx._2)))
        val result = sgd.predict(UNLABELED_LINEAR_DATA)
        val nResult = result.map(v => if (v > 0) 1.0 else -1.0)
        assert(arraysimilar(nResult, LABEL_LINEAR_DATA.map(_.toDouble), 0.9))
    }

    test("StochasticGradientDecent Test : Nonlinear Data - WRONG") {
        assert(sgd.clear())
        assert(sgd.config(Map[String, Double]()))
        assert(sgd.train(LABELED_NONLINEAR_DATA.map(yx => yx._1.toDouble -> yx._2)))
        val result = sgd.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arraysimilar(result, LABEL_NONLINEAR_DATA.map(_.toDouble), 0.45))
    }

    test("StochasticGradientDecent Test : Invalid Config & Data") {
        assert(sgd.clear())
        assert(!sgd.config(Map("limit" -> "test")))
        assert(!sgd.train(Array((1, Array(1, 2)), (1, Array()))))
    }
}
