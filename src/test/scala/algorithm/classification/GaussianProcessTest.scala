// Wei Chen - Gaussian Process Test
// 2016-11-24

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.general.MatrixFunc._
import ght.mi.algorithm.GaussianProcess

class GaussianProcessSuite extends FunSuite {

    val gp = new GaussianProcess()
    test("GaussianProcess Test : Initialization") {
        assert(gp.pointGroups.isEmpty)
    }

    test("GaussianProcess Test : Linear Train") {
        gp.train(LABELED_LINEAR_DATA)
        assert(gp.pointGroups.size == 2)
    }
    
    test("GaussianProcess Test : Linear Predict") {
        val result = gp.predict(UNLABELED_LINEAR_DATA, 3)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("GaussianProcess Test : Nonlinear Train") {
        gp.train(LABELED_NONLINEAR_DATA)
        assert(gp.pointGroups.size == 2)
    }
    
    test("GaussianProcess Test : Nonlinear Predict") {
        val result = gp.predict(UNLABELED_NONLINEAR_DATA, 3)
        assert(arrayequal(result, LABEL_NONLINEAR_DATA))
    }
    
    test("GaussianProcess Test : Clear") {
        gp.clear()
        assert(gp.pointGroups.isEmpty)
    }
}