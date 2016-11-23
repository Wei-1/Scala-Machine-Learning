// Wei Chen - Neural Network Test
// 2016-11-06

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.NeuralNetwork

class NeuralNetworkSuite extends FunSuite {

    val traindata_X: Array[Array[Double]] = Array(
        Array(1, 1, 0, 0, 1, 0, 1, 0),
        Array(1, 0, 0, 0, 1, 0, 1, 0),
        Array(1, 1, 0, 0, 1, 0, 1, 0),
        Array(0, 0, 1, 1, 0, 1, 1, 0),
        Array(0, 0, 1, 0, 0, 1, 1, 0),
        Array(0, 0, 1, 1, 0, 1, 1, 0)
    )

    val traindata_Y: Array[Array[Double]] = Array(
        Array(1, 0),
        Array(1, 0),
        Array(1, 0),
        Array(0, 1),
        Array(0, 1),
        Array(0, 1)
    )

    val testdata_X: Array[Array[Double]] = Array(
        Array(1, 1, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 1, 1, 0, 0, 0, 0)
    )

    val layer_neurons = Array(5, 4)
    val input_column = traindata_X.head.size
    val output_column = traindata_Y.head.size
    val limit = 1000

    val nn = new NeuralNetwork(layer_neurons, input_column, output_column)
    test("NeuralNetwork Test : Initialization") {
        assert(nn.syns(0).size == input_column)
        assert(nn.syns(layer_neurons.size).head.size == output_column)
    }

    test("NeuralNetwork Test : Train") {
        nn.train(traindata_X, traindata_Y, limit)
        assert(!nn.syns.isEmpty)
    }

    test("NeuralNetwork Test : Predict") {
        val result = nn.predict(testdata_X)
        assert(result(0)(0) > 0.5)
        assert(result(0)(1) < 0.5)
        assert(result(1)(0) < 0.5)
        assert(result(1)(1) > 0.5)    }
}