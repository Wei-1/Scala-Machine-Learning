// Wei Chen - Extreme Learning
// 2020-03-08

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class ExtremeLearning(val neuronNumber: Int, val featureNumber: Int, val outputNumber: Int) {
    var wIn = matrixrandom(featureNumber, neuronNumber, -1, 1)
    var wOut = matrixrandom(neuronNumber, outputNumber, -1, 1)

    def clear() = {
        wIn = matrixrandom(featureNumber, neuronNumber, -1, 1)
        wOut = matrixrandom(neuronNumber, outputNumber, -1, 1)
    }
    clear()

    private def reluLayer(x: Array[Array[Double]]): Array[Array[Double]] = {
        matrixdot(x, wIn).map(arr => arr.map(v => math.max(0, v)))
    }

    def train(x: Array[Array[Double]], y: Array[Array[Double]]) {
        val outX = reluLayer(x)
        val tranX = outX.transpose
        wOut = matrixdot(inverse(matrixdot(tranX, outX)), matrixdot(tranX, y))
    }

    def predict(x: Array[Array[Double]]): Array[Array[Double]] = {
        val outX = reluLayer(x)
        matrixdot(outX, wOut)
    }
}
