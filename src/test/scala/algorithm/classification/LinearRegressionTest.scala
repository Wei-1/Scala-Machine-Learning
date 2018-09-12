// Wei Chen - Linear Regression Test
// 2016-06-04

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.LinearRegression

class LinearRegressionSuite extends FunSuite {

    val linearregression = new LinearRegression()
    
    test("LinearRegression Test : Clear") {
        assert(linearregression.clear())
    }

    test("LinearRegression Test : Linear Data") {
        assert(linearregression.clear())
        assert(linearregression.config(Map[String, Double]()))
        assert(linearregression.train(LABELED_LINEAR_DATA))
        val result = linearregression.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("LinearRegression Test : Nonlinear Data - WRONG") {
        assert(linearregression.clear())
        assert(linearregression.config(Map[String, Double]()))
        assert(linearregression.train(LABELED_NONLINEAR_DATA))
        val result = linearregression.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }
}
