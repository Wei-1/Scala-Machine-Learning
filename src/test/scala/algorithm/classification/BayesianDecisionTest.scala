// Wei Chen - Bayesian Decision Test
// 2016-06-03

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.BayesianDecision

class BayesianDecisionSuite extends FunSuite {
    val traindata: Array[(Int, Array[Double])] = Array(
        (1, Array(2,2)),
        (1, Array(6,6)),
        (1, Array(0,0)),
        (1, Array(8,8)),
        (2, Array(2,6)),
        (2, Array(6,2)),
        (2, Array(0,8)),
        (2, Array(8,0)))

    val predictdata: Array[Array[Double]] = Array(
        Array(1,1),
        Array(3,3),
        Array(5,5),
        Array(7,7),
        Array(1,7),
        Array(7,1),
        Array(3,5),
        Array(5,3))

    val bayesiandecision = new BayesianDecision()
    test("BayesianDecision Test : Initialization") {
        val groupcnt = bayesiandecision.groupcnt
        val groupavg = bayesiandecision.groupavg
        val groupcov = bayesiandecision.groupcov
        assert(groupcnt.isEmpty)
        assert(groupavg.isEmpty)
        assert(groupcov.isEmpty)
    }

    test("BayesianDecision Test : Train") {
        val trainresult = bayesiandecision.train(traindata)
        assert(trainresult(0) == trainresult(1))
    }

    test("BayesianDecision Test : Predict") {
        val result = bayesiandecision.predict(predictdata)
        assert(arrayequal(result.map(_.toDouble), Array(1,1,1,1,2,2,2,2)))
    }

    test("BayesianDecision Test : Clear") {
        bayesiandecision.clear()
        val groupcnt = bayesiandecision.groupcnt
        val groupavg = bayesiandecision.groupavg
        val groupcov = bayesiandecision.groupcov
        assert(groupcnt.isEmpty)
        assert(groupavg.isEmpty)
        assert(groupcov.isEmpty)
    }
}