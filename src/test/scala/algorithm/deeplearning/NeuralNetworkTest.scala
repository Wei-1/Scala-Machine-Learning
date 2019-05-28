// Wei Chen - Neural Network Test
// 2018-11-29

import org.scalatest.FunSuite
import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.NeuralNetwork

class NeuralNetworkSuite extends FunSuite {

    val hidden_layer = Array(5, 4, 3)
    val input_column = UNLABELED_LARGE_HIGH_DIM_DATA.head.size
    val output_column = TARGET_LARGE_HIGH_DIM_DATA.head.size
    val layer_neurons = input_column +: hidden_layer :+ output_column
    val limit = 20000
    val nn_learning_rate = 0.1

    val nn = new NeuralNetwork()
    test("NeuralNetwork Test : Initialization") {
        assert(nn.config(layer_neurons))
    }

    test("NeuralNetwork Test : Train") {
        assert(nn.train(UNLABELED_LARGE_HIGH_DIM_DATA, TARGET_LARGE_HIGH_DIM_DATA,
            iter = limit, _learningRate = nn_learning_rate))
        assert(!nn.network.isEmpty)
    }

    test("NeuralNetwork Test : Predict") {
        val result = nn.predict(UNLABELED_SMALL_HIGH_DIM_DATA)
        assert(matrixsimilar(result, TARGET_SMALL_HIGH_DIM_DATA, 0.5))
    }
}
