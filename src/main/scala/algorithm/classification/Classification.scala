// Wei Chen - Classification
// 2018-09-12

package com.interplanetarytech.algorithm

trait Classification extends Algorithm {
    val algotype: String = "Classification"
    def train(data: Array[(Int, Array[Double])]): Boolean
    def predict(data: Array[Array[Double]]): Array[Int]
}