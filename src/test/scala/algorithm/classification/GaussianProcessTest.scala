// Wei Chen - Gaussian Process Test
// 2016-11-24

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.GaussianProcess

class GaussianProcessSuite extends FunSuite {
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

    test("GaussianProcess Test : Initialization") {
        val test = new GaussianProcess()
        assert(test.pointGroups.isEmpty)
    }

    val gp = new GaussianProcess()
    test("GaussianProcess Test : Train") {
        gp.train(traindata)
        assert(gp.pointGroups.size == 2)
    }
    
    test("GaussianProcess Test : Predict") {
        val result = gp.predict(predictdata, 3)
        assert(arrayequal(result.map(_.toDouble), Array(-1,-1,1,1)))
    }
    
    test("GaussianProcess Test : Clear") {
        gp.clear()
        assert(gp.pointGroups.isEmpty)
    }
}