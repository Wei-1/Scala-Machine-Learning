// Wei Chen - Quadratic Discriminant Analysis
// 2017-09-01

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class QDA(d1: Array[Array[Double]], d2: Array[Array[Double]]) {

    val n1 = d1.size
    val n2 = d2.size
    val m1 = matrixaccumulate(d1).map(_ / n1)
    val m2 = matrixaccumulate(d2).map(_ / n2)

    val c1 = covariance(d1)
    val c2 = covariance(d2)
    val ic1 = inverse(c1)
    val ic2 = inverse(c2)
    val ln1 = Math.log(determinant(c1))
    val ln2 = Math.log(determinant(c2))

    def predict(data: Array[Array[Double]]): Array[Int] = {
        val dm1 = data.map(d => arrayminus(d, m1))
        val dm2 = data.map(d => arrayminus(d, m2))
        val arr1 = matrixmultiply(dm1, matrixdot(dm1, ic1)).map(_.sum)
        val arr2 = matrixmultiply(dm2, matrixdot(dm2, ic2)).map(_.sum)

        arr1.zip(arr2).map { case (s1, s2) =>
            if (s1 + ln1 > s2 + ln2) 2 else 1
        }
    }

}
