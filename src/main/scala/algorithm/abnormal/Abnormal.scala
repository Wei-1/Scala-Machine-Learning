// Wei Chen - Abnormal Detection
// 2022-03-04

package com.scalaml.algorithm

trait Abnormal extends Algorithm {
    val algotype: String = "Abnormal"
    def train(data: Array[Array[Double]]): Boolean
    def predict(data: Array[Array[Double]]): Array[Double]
}