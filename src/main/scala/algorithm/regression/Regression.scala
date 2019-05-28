// Wei Chen - Regression
// 2018-09-12

package com.interplanetarytech.algorithm

trait Regression extends Algorithm {
    val algotype: String = "Regression"
    def train(data: Array[(Double, Array[Double])]): Boolean
    def predict(data: Array[Array[Double]]): Array[Double]
}