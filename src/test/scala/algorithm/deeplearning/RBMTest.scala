// Wei Chen - Restricted Boltzmann Machine Test
// 2016-11-20

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.RBM

class RBMSuite extends FunSuite {

    val learning_rate: Double = 0.1
    val limit: Int = 100
    val k: Int = 1
    
    val visible_n: Int = UNLABELED_LARGE_HIGH_DIM_DATA.head.size
    val hidden_n: Int = 3

    test("RBM Test : Initialization") {
        val test = new RBM(visible_n, hidden_n)
        assert(test.hbias.sum == 0)
        assert(test.vbias.sum == 0)
        assert(!test.syns.isEmpty)
    }

    val rbm = new RBM(visible_n, hidden_n)
    test("RBM Test : Train") {
        rbm.train(UNLABELED_LARGE_HIGH_DIM_DATA, learning_rate, k, limit)
        assert(!arrayequal(rbm.hbias, new Array[Double](hidden_n)))
        assert(!arrayequal(rbm.vbias, new Array[Double](visible_n)))
        assert(!rbm.syns.isEmpty)
    }

    test("RBM Test : Reconstruct") {
        val h = rbm.forward(UNLABELED_SMALL_HIGH_DIM_DATA)
        val result = rbm.reconstruct(h)
        assert(matrixsimilar(result, SIMULATE_SMALL_HIGH_DIM_DATA, 0.4))
    }
    
    test("RBM Test : Clear") {
        rbm.clear()
        assert(rbm.hbias.sum == 0)
        assert(rbm.vbias.sum == 0)
        assert(!rbm.syns.isEmpty)
    }
}
