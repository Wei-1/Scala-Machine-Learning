// Wei Chen - Linear Regression Test
// 2016-06-04

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.LinearRegression

class LinearRegressionSuite extends FunSuite {
    val traindata: Array[(Int, Array[Double])] = Array(
        (-1, Array(2,5)),
        (-1, Array(3,4)),
        (-1, Array(4,5)),
        (1, Array(5,4)),
        (1, Array(6,5)),
        (1, Array(7,4)))

    val predictdata: Array[Array[Double]] = Array(
        Array(0,4),
        Array(1,4),
        Array(8,5),
        Array(9,5))

    test("LinearRegression Test : Initialization") {
        val test = new LinearRegression()
        assert(test.projector.isEmpty)
    }

    val linearregression = new LinearRegression()
    test("LinearRegression Test : Train") {
        linearregression.train(traindata)
        assert(linearregression.projector(0)._1 == -1)
        assert(linearregression.projector(0)._2 == 1)
    }
    
    test("LinearRegression Test : Predict") {
        val result = linearregression.predict(predictdata)
        assert(arrayequal(result.map(_.toDouble), Array(-1,-1,1,1)))
    }
    
    test("LinearRegression Test : Clear") {
        linearregression.clear()
        assert(linearregression.projector.isEmpty)
    }
}
