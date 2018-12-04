// Wei Chen - Deep Belief Network Test
// 2016-11-23

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm.DBN

class DBNSuite extends FunSuite {

    val learning_rate: Double = 0.1
    val limit: Int = 200
    val k: Int = 1

    val layer_rbms = Array(5, 4)
    val layer_nns = Array(3)
    val input_column = UNLABELED_LARGE_HIGH_DIM_DATA.head.size
    val output_column = TARGET_LARGE_HIGH_DIM_DATA.head.size

    test("DBN Test : Initialization") {
        val test = new DBN(layer_rbms, layer_nns, input_column, output_column)
        for (i <- 0 until layer_rbms.size) {
            assert(test.rbm_layers(i).hbias.sum == 0)
            assert(test.rbm_layers(i).vbias.sum == 0)
        }
        assert(test.nn.network.head.size == layer_rbms.last)
        assert(test.nn.network.last.size == output_column)
    }

    val dbn = new DBN(layer_rbms, layer_nns, input_column, output_column)
    test("DBN Test : Train") {
        dbn.train(UNLABELED_LARGE_HIGH_DIM_DATA, TARGET_LARGE_HIGH_DIM_DATA, learning_rate, k, limit)
        assert(!dbn.nn.network.isEmpty)
    }

    test("DBN Test : Predict") {
        val result = dbn.predict(UNLABELED_SMALL_HIGH_DIM_DATA)
        assert(result(0)(0) > 0.5)
        assert(result(0)(1) < 0.5)
        assert(result(1)(0) < 0.5)
        assert(result(1)(1) > 0.5)
    }
    
    test("DBN Test : Clear") {
        dbn.clear()
        for (i <- 0 until layer_rbms.size) {
            assert(dbn.rbm_layers(i).hbias.sum == 0)
            assert(dbn.rbm_layers(i).vbias.sum == 0)
        }
        assert(dbn.nn.network.head.size == layer_rbms.last)
        assert(dbn.nn.network.last.size == output_column)
    }
}
