// Wei Chen - Algorithm
// 2018-09-22

package com.interplanetarytech.algorithm

trait Algorithm {
    val algotype: String
    val algoname: String
    val version: String
    def clear: Boolean
    def config(paras: Map[String, Any]): Boolean
}