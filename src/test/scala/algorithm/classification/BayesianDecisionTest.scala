// Wei Chen - Bayesian Decision Test
// 2016-06-03

import org.scalatest.FunSuite
import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.BayesianDecision

class BayesianDecisionSuite extends FunSuite {

    val bayesiandecision = new BayesianDecision()

    test("BayesianDecision Test : Clear") {
        assert(bayesiandecision.clear())
    }

    test("BayesianDecision Test : Linear Data") {
        assert(bayesiandecision.clear())
        assert(bayesiandecision.config(Map[String, Double]()))
        assert(bayesiandecision.train(LABELED_LINEAR_DATA))
        val result = bayesiandecision.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("BayesianDecision Test : Nonlinear Data") {
        assert(bayesiandecision.clear())
        assert(bayesiandecision.config(Map[String, Double]()))
        assert(bayesiandecision.train(LABELED_NONLINEAR_DATA))
        val result = bayesiandecision.predict(UNLABELED_NONLINEAR_DATA)
        assert(arrayequal(result, LABEL_NONLINEAR_DATA))
    }
}
