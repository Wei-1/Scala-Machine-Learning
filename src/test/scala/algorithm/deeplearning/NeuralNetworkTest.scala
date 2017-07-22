// Wei Chen - Neural Network Test
// 2016-11-06

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.general.MatrixFunc._
import ght.mi.algorithm.NeuralNetwork

class NeuralNetworkSuite extends FunSuite {

    val layer_neurons = Array(5, 4)
    val input_column = UNLABELED_LARGE_HIGH_DIM_DATA.head.size
    val output_column = TARGET_LARGE_HIGH_DIM_DATA.head.size
    val limit = 1000

    val nn = new NeuralNetwork(layer_neurons, input_column, output_column)
    test("NeuralNetwork Test : Initialization") {
        assert(nn.syns(0).size == input_column)
        assert(nn.syns(layer_neurons.size).head.size == output_column)
    }

    test("NeuralNetwork Test : Train") {
        nn.train(UNLABELED_LARGE_HIGH_DIM_DATA, TARGET_LARGE_HIGH_DIM_DATA, limit)
        assert(!nn.syns.isEmpty)
    }

    test("NeuralNetwork Test : Predict") {
        val result = nn.predict(UNLABELED_SMALL_HIGH_DIM_DATA)
        assert(matrixsimilar(result, TARGET_SMALL_HIGH_DIM_DATA, 0.2))
    }
}