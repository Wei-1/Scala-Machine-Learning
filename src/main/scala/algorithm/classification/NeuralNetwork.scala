// Wei Chen - NN - Neural Network with basic sigmoid
// 2016-08-29

package ght.mi.algorithm
import ght.mi.algorithm.MatrixFunc._

/*
val X = Array(Array(0.0,0.0,1.0), Array(0.0,1.0,1.0), Array(1.0,0.0,1.0), Array(1.0,1.0,1.0))
val Y = Array(Array(0.0, 1.0), Array(1.0, 0.0), Array(1.0, 0.0), Array(0.0, 1.0))
val layer_neurons = Array(5, 4)
val nn = new NeuralNetwork(layer_neurons)
nn.input(X, Y)
nn.train(1000)
val result = nn.predict(X)
println(result.map(_(0)).mkString(" "))
println(result.map(_(1)).mkString(" "))
*/

class NeuralNetwork(val layer_neurons: Array[Int]) {
    var X = Array[Array[Double]]()
    var Y = Array[Array[Double]]()
    var syns = Array[Array[Array[Double]]]()
    val layer_number = layer_neurons.size

    private def neuron(x: Double, forward: Boolean): Double = {
        if (forward) return 1 / (1 + Math.exp(-x))
        else return x * (1-x)
    }

    private def neuralLayer(x: Array[Array[Double]], forward: Boolean): Array[Array[Double]] = {
        x.map(arr => arr.map(n => neuron(n, forward)))
    }

    def input(x: Array[Array[Double]], y: Array[Array[Double]]) {
        X = x
        Y = y
        val input_column = x.head.size
        val output_column = y.head.size
        syns = Array.fill(layer_number+1)(Array[Array[Double]]())
        syns(0) = matrixrandom(input_column, layer_neurons(0), -1, 1)
        for (i <- 0 to layer_number-2) {
            syns(i+1) = matrixrandom(layer_neurons(i), layer_neurons(i+1), -1, 1)
        }
        syns(layer_number) = matrixrandom(layer_neurons(layer_number-1), output_column, -1, 1)
    }

    def iterate() {
        var layer_results = Array.fill(layer_number+2)(Array[Array[Double]]())
        layer_results(0) = X
        for (i <- 0 to layer_number) {
            layer_results(i+1) =  neuralLayer(matrixdot(layer_results(i), syns(i)), true)
        }

        var layer_deltas = Array.fill(layer_number+1)(Array[Array[Double]]())
        val layer_error = matrixminus(Y, layer_results(layer_number+1))
        layer_deltas(layer_number) = matrixmultiply(layer_error, neuralLayer(layer_results(layer_number+1), false))
        for (i <- layer_number-1 to 0 by -1) {
            val layer_error = matrixdot(layer_deltas(i+1), syns(i+1).transpose)
            layer_deltas(i) = matrixmultiply(layer_error, neuralLayer(layer_results(i+1), false))
        }

        for (i <- layer_number to 0 by -1) {
            syns(i) = matrixsum(syns(i), matrixdot(layer_results(i).transpose, layer_deltas(i)))
        }
    }

    def train(iterate_limit: Int) = for (i <- 1 to iterate_limit) iterate()

    def predict(x: Array[Array[Double]]): Array[Array[Double]] = {
        var layer_result = x
        for (i <- 0 to layer_number) {
            layer_result = neuralLayer(matrixdot(layer_result, syns(i)), true)
        }
        return layer_result
    }

    def clear() {
        syns = Array[Array[Array[Double]]]()
    }
}
