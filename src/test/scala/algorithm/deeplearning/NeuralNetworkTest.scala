// Wei Chen - Neural Network Test
// 2018-11-29

import org.scalatest.FunSuite
import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.NeuralNetwork
import com.scalaml.algorithm.{TANH, SIGMOID, RELU, LINEAR, SQUARE, L1, L2}

class NeuralNetworkSuite extends FunSuite {

    val hidden_layer = Array(5, 4, 3)
    val input_column = UNLABELED_LARGE_HIGH_DIM_DATA.head.size
    val output_column = TARGET_LARGE_HIGH_DIM_DATA.head.size
    val layer_neurons = input_column +: hidden_layer :+ output_column
    val limit = 20000
    val nn_learning_rate = 0.05
    val inputIds = (for(i <- 0 until input_column) yield ('a' + i).toString).toArray

    val nn = new NeuralNetwork()
    test("NeuralNetwork Test : SIGMOID Initialization") { // most stable in small data
        assert(nn.config(layer_neurons, SIGMOID, LINEAR, null, inputIds))
    }

    test("NeuralNetwork Test : SIGMOID Training") {
        assert(nn.train(UNLABELED_LARGE_HIGH_DIM_DATA, TARGET_LARGE_HIGH_DIM_DATA,
            iter = limit, _learningRate = nn_learning_rate))
        assert(!nn.network.isEmpty)
    }

    test("NeuralNetwork Test : SIGMOID Predict") {
        val result = nn.predict(UNLABELED_SMALL_HIGH_DIM_DATA)
        val loss = result.zip(TARGET_SMALL_HIGH_DIM_DATA).map { case (a1, a2) =>
            arrayminussquare(a1, a2).sum / result.head.size
        }.sum / result.size
        Console.err.println("SIGMOID: " + loss)
        assert(matrixsimilar(result, TARGET_SMALL_HIGH_DIM_DATA, 0.5))
    }

    test("NeuralNetwork Test : TANH Activation Functions") {
        val nn2 = new NeuralNetwork()
        assert(nn2.config(layer_neurons, TANH))
        assert(nn2.train(UNLABELED_LARGE_HIGH_DIM_DATA, TARGET_LARGE_HIGH_DIM_DATA,
            iter = limit, _learningRate = nn_learning_rate))
        val result = nn2.predict(UNLABELED_SMALL_HIGH_DIM_DATA)
        val loss = result.zip(TARGET_SMALL_HIGH_DIM_DATA).map { case (a1, a2) =>
            arrayminussquare(a1, a2).sum / result.head.size
        }.sum / result.size
        Console.err.println("TANH: " + loss)
        assert(loss < 0.5)
        // assert(matrixsimilar(result, TARGET_SMALL_HIGH_DIM_DATA, 0.5))
    }

    test("NeuralNetwork Test : RELU Activation Functions") { // not stable
        val nn2 = new NeuralNetwork()
        assert(nn2.config(layer_neurons, RELU))
        assert(nn2.train(UNLABELED_LARGE_HIGH_DIM_DATA, TARGET_LARGE_HIGH_DIM_DATA,
            iter = limit * 2, _learningRate = nn_learning_rate / 5))
        val result = nn2.predict(UNLABELED_SMALL_HIGH_DIM_DATA)
        val loss = result.zip(TARGET_SMALL_HIGH_DIM_DATA).map { case (a1, a2) =>
            arrayminussquare(a1, a2).sum / result.head.size
        }.sum / result.size
        Console.err.println("RELU: " + loss)
        assert(loss < 0.5)
        // assert(matrixsimilar(result, TARGET_SMALL_HIGH_DIM_DATA, 0.5))
    }

    test("NeuralNetwork Test : RELU Activation Functions - L1") { // more stable + dead cell
        val nn2 = new NeuralNetwork()
        assert(nn2.config(layer_neurons, RELU, LINEAR, L1))
        assert(nn2.train(UNLABELED_LARGE_HIGH_DIM_DATA, TARGET_LARGE_HIGH_DIM_DATA,
            iter = limit * 2, _learningRate = nn_learning_rate / 5))
        val result = nn2.predict(UNLABELED_SMALL_HIGH_DIM_DATA)
        val loss = result.zip(TARGET_SMALL_HIGH_DIM_DATA).map { case (a1, a2) =>
            arrayminussquare(a1, a2).sum / result.head.size
        }.sum / result.size
        Console.err.println("RELU L1: " + loss)
        assert(loss < 0.5)
        // assert(matrixsimilar(result, TARGET_SMALL_HIGH_DIM_DATA, 0.5))
    }

    test("NeuralNetwork Test : RELU Activation Functions - L2") { // most stable
        val nn2 = new NeuralNetwork()
        assert(nn2.config(layer_neurons, RELU, LINEAR, L2))
        assert(nn2.train(UNLABELED_LARGE_HIGH_DIM_DATA, TARGET_LARGE_HIGH_DIM_DATA,
            iter = limit * 2, _learningRate = nn_learning_rate / 5))
        val result = nn2.predict(UNLABELED_SMALL_HIGH_DIM_DATA)
        val loss = result.zip(TARGET_SMALL_HIGH_DIM_DATA).map { case (a1, a2) =>
            arrayminussquare(a1, a2).sum / result.head.size
        }.sum / result.size
        Console.err.println("RELU L2: " + loss)
        assert(loss < 0.5)
        // assert(matrixsimilar(result, TARGET_SMALL_HIGH_DIM_DATA, 0.5))
    }

    test("NeuralNetwork Test : Invalid Config") {
        assert(!nn.config(null))
    }

    test("NeuralNetwork Test : Unit Functions") {
        assert(SQUARE.error(1, 1) == 0)
        assert(SQUARE.error(1, 0) == 0.5)
        assert(L1.output(1) == 1)
        assert(L1.output(-1) == 1)
        assert(L1.der(1) == 1)
        assert(L1.der(-1) == -1)
        assert(L2.output(1) == 0.5)
        assert(L2.output(-1) == 0.5)
        assert(L2.der(1) == 1)
        assert(L2.der(-1) == -1)
    }
}
