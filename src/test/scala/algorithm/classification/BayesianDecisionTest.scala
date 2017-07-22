// Wei Chen - Bayesian Decision Test
// 2016-06-03

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.general.MatrixFunc._
import ght.mi.algorithm.BayesianDecision

class BayesianDecisionSuite extends FunSuite {

    val bayesiandecision = new BayesianDecision()
    test("BayesianDecision Test : Initialization") {
        assert(bayesiandecision.groupcnt.isEmpty)
        assert(bayesiandecision.groupavg.isEmpty)
        assert(bayesiandecision.groupcov.isEmpty)
    }

    test("BayesianDecision Test : Linear Train") {
        val trainresult = bayesiandecision.train(LABELED_LINEAR_DATA)
        assert(trainresult(0) == trainresult(1))
    }

    test("BayesianDecision Test : Linear Predict") {
        val result = bayesiandecision.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("BayesianDecision Test : Nonlinear Train") {
        val trainresult = bayesiandecision.train(LABELED_NONLINEAR_DATA)
        assert(trainresult(0) == trainresult(1))
    }

    test("BayesianDecision Test : Nonlinear Predict") {
        val result = bayesiandecision.predict(UNLABELED_NONLINEAR_DATA)
        assert(arrayequal(result, LABEL_NONLINEAR_DATA))
    }

    test("BayesianDecision Test : Clear") {
        bayesiandecision.clear()
        assert(bayesiandecision.groupcnt.isEmpty)
        assert(bayesiandecision.groupavg.isEmpty)
        assert(bayesiandecision.groupcov.isEmpty)
    }
}