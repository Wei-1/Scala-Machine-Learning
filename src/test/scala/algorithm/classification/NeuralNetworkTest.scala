// Wei Chen - Neural Network Test
// 2016-11-06

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.NeuralNetwork

class NeuralNetworkSuite extends FunSuite {
    val traindata_X = Array(
        Array(0.0,0.0,1.0),
        Array(0.0,1.0,1.0),
        Array(1.0,0.0,1.0),
        Array(1.0,1.0,1.0))
    val traindata_Y = Array(
        Array(0.0, 1.0),
        Array(1.0, 0.0),
        Array(1.0, 0.0),
        Array(0.0, 1.0))
    val predictdata = Array(
        Array(0.0,0.0,1.0),
        Array(0.0,1.0,1.0),
        Array(1.0,0.0,1.0),
        Array(1.0,1.0,1.0))
    val layer_neurons = Array(5, 4)
    val limit = 1000

    test("NeuralNetwork Test : Initialization") {
        val test = new NeuralNetwork(layer_neurons)
        assert(test.syns.isEmpty)
    }

    val nn = new NeuralNetwork(layer_neurons)
    test("NeuralNetwork Test : Input") {
        nn.input(traindata_X, traindata_Y)
        assert(!nn.syns.isEmpty)
    }

    test("NeuralNetwork Test : Train") {
        nn.train(limit)
        assert(!nn.syns.isEmpty)
    }

    test("NeuralNetwork Test : Predict") {
        val result = nn.predict(predictdata)
        assert(result(0)(0) < 0.5)
        assert(result(0)(1) > 0.5)
        assert(result(1)(0) > 0.5)
        assert(result(1)(1) < 0.5)
        assert(result(2)(0) > 0.5)
        assert(result(2)(1) < 0.5)
        assert(result(3)(0) < 0.5)
        assert(result(3)(1) > 0.5)
    }
    
    test("NeuralNetwork Test : Clear") {
        nn.clear()
        assert(nn.syns.isEmpty)
    }
}