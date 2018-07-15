// Wei Chen - DBN - Deep Belief Network
// 2016-11-23

package com.interplanetarytech.algorithm

class DBN(val layer_rbms: Array[Int], val layer_nns: Array[Int], val input_column: Int, val output_column: Int) {
    val rbm_number = layer_rbms.size
    val nn_number = layer_nns.size

    val rbm_layers: Array[RBM] = new Array[RBM](layer_rbms.size)
    val nn = new NeuralNetwork(layer_nns, layer_rbms.last, output_column)

    def clear() {
        rbm_layers(0) = new RBM(input_column, layer_rbms(0))
        for (i <- 1 until rbm_number) {
            rbm_layers(i) = new RBM(layer_rbms(i-1), layer_rbms(i))
        }
        nn.clear
    }
    clear()

    def train(x: Array[Array[Double]], y: Array[Array[Double]], lr: Double, k: Int, limit: Int) {
        var layer_input = x
        for (i <- 0 until rbm_number) {
            for (j <- 0 until limit) {
                rbm_layers(i).train(layer_input, lr, k, limit)
            }
            layer_input = rbm_layers(i).forward(layer_input)
        }
        nn.train(layer_input, y, limit, lr)
    }

    def predict(x: Array[Array[Double]]): Array[Array[Double]] = {
        var layer_input = x
        for (i <- 0 until rbm_number) {
            layer_input = rbm_layers(i).forward(layer_input)
        }
        return nn.predict(layer_input)
    }
}
