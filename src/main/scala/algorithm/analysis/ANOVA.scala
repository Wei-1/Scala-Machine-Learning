// Wei Chen - ANOVA
// 2017-08-30

package com.interplanetarytech.algorithm

class ANOVA(data: Array[Array[Double]]) {

    private val n = data.size // number of groups

    private val avg_i = data.map(a => a.sum / a.size) // average of each group
    private val avg_a = avg_i.sum / n // average all

    // between-groups sum of squares
    val ssg: Double = data.zip(avg_i).map { case (a, i) => a.size * Math.pow(i - avg_a, 2) }.sum
    // within-groups sum of squares (error)
    val sse: Double = data.zip(avg_i).map { case (a, i) => a.map(d => Math.pow(d - i, 2)).sum }.sum

    // between-groups' degree of freedom
    val dfg: Double = n - 1
    // within-groups' degree of freedom (error)
    val dfe: Double = data.map(_.size).sum - n

    // between-groups mean square
    val msg: Double = ssg / dfg
    // within-groups mean square (error)
    val mse: Double = sse / dfe

    // f value
    val f: Double = msg / mse
}
