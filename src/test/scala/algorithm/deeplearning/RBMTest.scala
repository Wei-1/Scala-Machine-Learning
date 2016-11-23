// Wei Chen - Restricted Boltzmann Machine Test
// 2016-11-20

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.RBM

class RBMSuite extends FunSuite {
    
    val traindata_X: Array[Array[Double]] = Array(
        Array(1, 1, 0, 0, 1, 0, 1, 0),
        Array(1, 0, 0, 0, 1, 0, 1, 0),
        Array(1, 1, 0, 0, 1, 0, 1, 0),
        Array(0, 0, 1, 1, 0, 1, 1, 0),
        Array(0, 0, 1, 0, 0, 1, 1, 0),
        Array(0, 0, 1, 1, 0, 1, 1, 0)
    )

    val testdata_X: Array[Array[Double]] = Array(
        Array(1, 1, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 1, 1, 0, 0, 0, 0)
    )

    val learning_rate: Double = 0.1
    val limit: Int = 100
    val k: Int = 1
    
    val visible_n: Int = traindata_X.head.size
    val hidden_n: Int = 3

    test("RBM Test : Initialization") {
        val test = new RBM(visible_n, hidden_n)
        assert(test.hbias.sum == 0)
        assert(test.vbias.sum == 0)
        assert(!test.syns.isEmpty)
    }

    val rbm = new RBM(visible_n, hidden_n)
    test("RBM Test : Train") {
        rbm.train(traindata_X, learning_rate, k, limit)
        assert(!arrayequal(rbm.hbias, new Array[Double](hidden_n)))
        assert(!arrayequal(rbm.vbias, new Array[Double](visible_n)))
        assert(!rbm.syns.isEmpty)
    }

    test("RBM Test : Reconstruct") {
        val h = rbm.forward(testdata_X)
        val result = rbm.reconstruct(h)
        assert(result(0)(4) > 0.5)
        assert(result(0)(5) < 0.5)
        assert(result(1)(4) < 0.5)
        assert(result(1)(5) > 0.5)
    }
    
    test("RBM Test : Clear") {
        rbm.clear()
        assert(rbm.hbias.sum == 0)
        assert(rbm.vbias.sum == 0)
        assert(!rbm.syns.isEmpty)
    }
}