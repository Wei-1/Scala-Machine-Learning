// Wei Chen - Clustering
// 2018-09-30

package com.interplanetarytech.algorithm

trait Clustering extends Algorithm {
    val algotype: String = "Clustering"
    def cluster(data: Array[Array[Double]]): Array[Int]
}