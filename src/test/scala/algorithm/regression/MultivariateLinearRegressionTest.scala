// Wei Chen - Multivariate Linear Regression Test
// 2016-06-04

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.MultivariateLinearRegression

class MultivariateLinearRegressionSuite extends FunSuite {

    val mlr = new MultivariateLinearRegression()
    
    test("MultivariateLinearRegression Test : Clear") {
        assert(mlr.clear())
    }

    test("MultivariateLinearRegression Test : Linear Data") {
        assert(mlr.clear())
        assert(mlr.config(Map[String, Double]()))
        assert(mlr.train(LABELED_LINEAR_DATA.map(yx => yx._1.toDouble -> yx._2)))
        val result = mlr.predict(UNLABELED_LINEAR_DATA)
        assert(arraysimilar(result, LABEL_LINEAR_DATA.map(_.toDouble), 0.9))
    }

    test("MultivariateLinearRegression Test : Nonlinear Data - WRONG") {
        assert(mlr.clear())
        assert(mlr.config(Map[String, Double]()))
        assert(mlr.train(LABELED_NONLINEAR_DATA.map(yx => yx._1.toDouble -> yx._2)))
        val result = mlr.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arraysimilar(result, LABEL_LINEAR_DATA.map(_.toDouble), 0.45))
    }
}
