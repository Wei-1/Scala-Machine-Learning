// Wei Chen - Linear Regression Test
// 2016-06-04

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.LinearRegression

class LinearRegressionSuite extends FunSuite {

    val linearregression = new LinearRegression()
    test("LinearRegression Test : Initialization") {
        assert(linearregression.projector.isEmpty)
    }

    test("LinearRegression Test : Linear Train") {
        linearregression.train(LABELED_LINEAR_DATA)
        assert(linearregression.projector(0)._1 == -1)
        assert(linearregression.projector(0)._2 == 1)
    }
    
    test("LinearRegression Test : Linear Predict") {
        val result = linearregression.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }
    
    test("LinearRegression Test : Clear") {
        linearregression.clear()
        assert(linearregression.projector.isEmpty)
    }

    test("LinearRegression Test : Nonlinear Train") {
        linearregression.train(LABELED_NONLINEAR_DATA)
        assert(linearregression.projector(0)._1 == 1)
        assert(linearregression.projector(0)._2 == 2)
    }
    
    test("LinearRegression Test : Nonlinear Predict - WRONG") {
        val result = linearregression.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }
}
