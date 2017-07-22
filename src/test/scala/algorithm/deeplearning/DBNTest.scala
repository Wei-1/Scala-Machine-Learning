// Wei Chen - Deep Belief Network Test
// 2016-11-23

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.general.MatrixFunc._
import ght.mi.algorithm.DBN

class DBNSuite extends FunSuite {

    val learning_rate: Double = 0.1
    val limit: Int = 100
    val k: Int = 1

    val layer_rbms = Array(5, 3)
    val layer_nns = Array(3)
    val input_column = UNLABELED_LARGE_HIGH_DIM_DATA.head.size
    val output_column = TARGET_LARGE_HIGH_DIM_DATA.head.size

    test("DBN Test : Initialization") {
        val test = new DBN(layer_rbms, layer_nns, input_column, output_column)
        for (i <- 0 until layer_rbms.size) {
            assert(test.rbm_layers(i).hbias.sum == 0)
            assert(test.rbm_layers(i).vbias.sum == 0)
        }
        assert(test.nn.syns(0).size == layer_rbms.last)
        assert(test.nn.syns(layer_nns.size).head.size == output_column)
    }

    val dbn = new DBN(layer_rbms, layer_nns, input_column, output_column)
    test("DBN Test : Train") {
        dbn.train(UNLABELED_LARGE_HIGH_DIM_DATA, TARGET_LARGE_HIGH_DIM_DATA, learning_rate, k, limit)
        assert(!dbn.nn.syns.isEmpty)
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
        assert(dbn.nn.syns(0).size == layer_rbms.last)
        assert(dbn.nn.syns(layer_nns.size).head.size == output_column)
    }
}