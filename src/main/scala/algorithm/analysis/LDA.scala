// Wei Chen - Linear Discriminant Analysis
// 2017-09-01

package com.interplanetarytech.algorithm
import com.interplanetarytech.general.MatrixFunc._

class LDA(d1: Array[Array[Double]], d2: Array[Array[Double]]) {

    val n1 = d1.size
    val n2 = d2.size
    val m1 = matrixaccumulate(d1).map(_ / n1)
    val m2 = matrixaccumulate(d2).map(_ / n2)

    val c1 = covariance(d1)
    val c2 = covariance(d2)
    val ic = inverse(matrixsum(c1, c2))

    val w = matrixdot(ic, arrayminus(m2, m1).map(m => Array(m))).map(_.head)
    val c = arraymultiply(arraysum(m1, m2).map(m => m / 2), w).sum

    def predict(data: Array[Array[Double]]): Array[Int] =
        data.map(d => if (arraymultiply(d, w).sum > c) 2 else 1)

}
