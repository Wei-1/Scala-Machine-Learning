// Wei Chen - Student T
// 2017-07-25

package com.interplanetarytech.algorithm

class StudentT {

    private def arrayminusonesquaresum(arr: Array[Double], avg: Double): Double =
        arr.map(a => Math.pow(a - avg, 2)).sum

    def oneSample(arr: Array[Double], v: Double): Double = {
        val asize = arr.size
        if (asize == 0) {
            0.0
        } else {
            val avg = arr.sum / asize
            (avg - v).abs * asize / Math.sqrt(arrayminusonesquaresum(arr, avg))
        }
    }

    def twoSample(arr1: Array[Double], arr2: Array[Double]): Double = {
        val asize1 = arr1.size
        val asize2 = arr2.size
        if (asize1 == 0 || asize2 == 0) {
            0.0
        } else {
            val avg1 = arr1.sum / asize1
            val avg2 = arr2.sum / asize2
            val std1_2 = arrayminusonesquaresum(arr1, avg1) / asize1
            val std2_2 = arrayminusonesquaresum(arr2, avg2) / asize2
            (avg1 - avg2).abs / Math.sqrt(std1_2 / asize1 + std2_2 / asize2)
        }
    }
}
