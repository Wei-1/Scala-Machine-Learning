// Wei Chen - Bayesian Decision Test
// 2016-06-03

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.BayesianDecision

class BayesianDecisionSuite extends FunSuite {
    val traindata = Array(
        Array(1,2,2),
        Array(1,6,6),
        Array(1,0,0),
        Array(1,8,8),
        Array(2,2,6),
        Array(2,6,2),
        Array(2,0,8),
        Array(2,8,0)
    ).map(d => (d(0), d.drop(1).map(_.toDouble)))
    val predictdata: Array[Array[Double]] = Array(
        Array(1,1),
        Array(3,3),
        Array(5,5),
        Array(7,7),
        Array(1,7),
        Array(7,1),
        Array(3,5),
        Array(5,3)
    )

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