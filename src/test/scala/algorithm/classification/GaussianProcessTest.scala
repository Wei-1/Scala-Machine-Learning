// Wei Chen - Gaussian Process Test
// 2016-11-24

import org.scalatest.FunSuite
import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.GaussianProcess

class GaussianProcessSuite extends FunSuite {

    val gp = new GaussianProcess()

    test("GaussianProcess Test : Clear") {
        assert(gp.clear())
    }

    test("GaussianProcess Test : Linear Data") {
        assert(gp.clear())
        assert(gp.config(Map("std" -> 3.0)))
        assert(gp.train(LABELED_LINEAR_DATA))
        assert(gp.pointGroups.size == 2)
        val result = gp.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }
    
    test("GaussianProcess Test : Nonlinear Data") {
        assert(gp.clear())
        assert(gp.config(Map("std" -> 3.0)))
        assert(gp.train(LABELED_NONLINEAR_DATA))
        val result = gp.predict(UNLABELED_NONLINEAR_DATA)
        assert(arrayequal(result, LABEL_NONLINEAR_DATA))
    }
}
