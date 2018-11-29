// Wei Chen - NN - Neural Network with basic sigmoid
// 2016-08-29

package com.interplanetarytech.algorithm
import com.interplanetarytech.general.MatrixFunc._

class BasicNN(val layer_neurons: Array[Int], val input_column: Int, val output_column: Int) {
    val layer_number = layer_neurons.size
    val syns = Array.fill(layer_number+1)(Array[Array[Double]]())

    def clear() = {
        syns(0) = matrixrandom(input_column, layer_neurons(0), -1, 1)
        for (i <- 0 to layer_number-2) {
            syns(i+1) = matrixrandom(layer_neurons(i), layer_neurons(i+1), -1, 1)
        }
        syns(layer_number) = matrixrandom(layer_neurons(layer_number-1), output_column, -1, 1)
    }
    clear()

    private def neuron(x: Double, forward: Boolean): Double = {
        if (forward) return 1 / (1 + Math.exp(-x))
        else return x * (1-x)
    }

    private def neuralLayer(x: Array[Array[Double]], forward: Boolean): Array[Array[Double]] = {
        x.map(arr => arr.map(n => neuron(n, forward)))
    }

    private def linearBack(x: Array[Array[Double]]): Array[Array[Double]] = {
        x.map(arr => arr.map(n => 1.0))
    }

    def iterate(x: Array[Array[Double]], y: Array[Array[Double]], lr: Double) {
        var layer_results = Array.fill(layer_number+2)(Array[Array[Double]]())
        layer_results(0) = x
        for (i <- 0 until layer_number) {
            layer_results(i+1) =  neuralLayer(matrixdot(layer_results(i), syns(i)), true)
        }
        layer_results(layer_number+1) = matrixdot(layer_results(layer_number), syns(layer_number))

        var layer_deltas = Array.fill(layer_number+1)(Array[Array[Double]]())

        val layer_error = matrixminus(y, layer_results(layer_number+1))
        layer_deltas(layer_number) = layer_error // matrixmultiply(layer_error, linearBack(layer_results(layer_number+1)))
        for (i <- layer_number-1 to 0 by -1) {
            val layer_error = matrixdot(layer_deltas(i+1), syns(i+1).transpose)
            layer_deltas(i) = matrixmultiply(layer_error, neuralLayer(layer_results(i+1), false))
        }

        for (i <- layer_number to 0 by -1) {
            syns(i) = matrixsum(syns(i), matrixdot(layer_results(i).transpose, layer_deltas(i)).map(_.map(_ * lr)))
        }
    }

    def train(x: Array[Array[Double]], y: Array[Array[Double]], iterate_limit: Int, lr: Double = 1) =
        for (i <- 1 to iterate_limit) iterate(x, y, lr)

    def predict(x: Array[Array[Double]]): Array[Array[Double]] = {
        var layer_input = x
        for (i <- 0 until layer_number) {
            layer_input = neuralLayer(matrixdot(layer_input, syns(i)), true)
        }
        return matrixdot(layer_input, syns(layer_number))
    }
}
