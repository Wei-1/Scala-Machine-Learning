// Wei Chen - Multiple Linear Regression Test
// 2016-06-04

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.MultipleLinearRegression
import org.scalatest.funsuite.AnyFunSuite

class MultipleLinearRegressionSuite extends AnyFunSuite {

    val mlr = new MultipleLinearRegression()
    
    test("MultipleLinearRegression Test : Clear") {
        assert(mlr.clear())
    }

    test("MultipleLinearRegression Test : Linear Data") {
        assert(mlr.clear())
        assert(mlr.config(Map[String, Double]()))
        assert(mlr.train(LABELED_LINEAR_DATA.map(yx => yx._1.toDouble -> yx._2)))
        val result = mlr.predict(UNLABELED_LINEAR_DATA)
        assert(arraysimilar(result, LABEL_LINEAR_DATA.map(_.toDouble), 0.9))
    }

    test("MultipleLinearRegression Test : Nonlinear Data - WRONG") {
        assert(mlr.clear())
        assert(mlr.config(Map[String, Double]()))
        assert(mlr.train(LABELED_NONLINEAR_DATA.map(yx => yx._1.toDouble -> yx._2)))
        val result = mlr.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arraysimilar(result, LABEL_LINEAR_DATA.map(_.toDouble), 0.45))
    }

    test("MultipleLinearRegression Test : Invalid Data") {
        assert(mlr.clear())
        assert(!mlr.train(Array((1, Array(1, 2)), (1, Array()))))
    }
}
