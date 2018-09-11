// Wei Chen - Classifier
// 2018-09-12

package com.interplanetarytech.algorithm

trait Classifier {
    def clear(): Boolean

    def train(data: Array[(Int, Array[Double])], paras: Map[String, Double]): Boolean

    def predict(data: Array[Array[Double]]): Array[Int]
}
