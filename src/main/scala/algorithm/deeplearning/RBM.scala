// Wei Chen - Restricted Boltzmann Machine
// 2016-11-20

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class RBM(val visible_n: Int, val hidden_n: Int) {

    val a: Double = 1.0 / visible_n
    var syns: Array[Array[Double]] = matrixrandom(hidden_n, visible_n, -a, a)
    var hbias: Array[Double] = new Array[Double](hidden_n)
    var vbias: Array[Double] = new Array[Double](visible_n)

    def clear() {
        syns = matrixrandom(hidden_n, visible_n, -a, a)
        hbias = new Array[Double](hidden_n)
        vbias = new Array[Double](visible_n)
    }

    private def sigmoid(x: Double): Double = 1 / (1 + Math.exp(-x))

    private def propUp(v: Array[Double], j: Int, b: Double): Double = {
        var temp: Double = b
        for (i <- 0 until visible_n) temp += syns(j)(i) * v(i)
        sigmoid(temp)
    }

    private def propDown(h: Array[Double], i: Int, b: Double): Double = {
        var temp: Double = b
        for (j <- 0 until hidden_n) temp += syns(j)(i) * h(j)
        sigmoid(temp)
    }

    private def oneForward(v_sample: Array[Double], h_sample: Array[Double]) = // v to h
        for (j <- 0 until hidden_n) h_sample(j) = propUp(v_sample, j, hbias(j))

    private def oneReconstruct(h_sample: Array[Double], v_sample: Array[Double]) = // h to v
        for (i <- 0 until visible_n) v_sample(i) = propDown(h_sample, i, vbias(i))
    
    private def onetrain(xi: Array[Double], lr: Double, k: Int) {
        val ph_samples = new Array[Double](hidden_n)
        val nv_samples = new Array[Double](visible_n)
        val nh_samples = new Array[Double](hidden_n)

        // Contrastive Divergence (CD) - k
        oneForward(xi, ph_samples)
        for (i <- 0 until k) {
            if (i == 0) oneReconstruct(ph_samples, nv_samples)
            else oneReconstruct(nh_samples, nv_samples)
            oneForward(nv_samples, nh_samples)
        }

        for (i <- 0 until hidden_n) {
            for (j <- 0 until visible_n) {
                syns(i)(j) += lr * (ph_samples(i) * xi(j) - nh_samples(i) * nv_samples(j))
            }
            hbias(i) += lr * (ph_samples(i) - nh_samples(i))
        }

        for (i <- 0 until visible_n) {
            vbias(i) += lr * (xi(i) - nv_samples(i))
        }
    }

    def iterate(x: Array[Array[Double]], lr: Double, k: Int) =
        for (xi <- x) onetrain(xi, lr, k)

    def train(x: Array[Array[Double]], lr: Double, k: Int, limit: Int) =
        for (i <- 0 until limit) iterate(x, lr, k)

    def forward(x: Array[Array[Double]]): Array[Array[Double]] = { // v to h
        val result: Array[Array[Double]] = Array.ofDim[Double](x.size, hidden_n)
        for (i <- 0 until x.size) oneForward(x(i), result(i))
        return result
    }

    def reconstruct(y: Array[Array[Double]]): Array[Array[Double]] = { // h to v
        val result: Array[Array[Double]] = Array.ofDim[Double](y.size, visible_n)
        for (i <- 0 until y.size) oneReconstruct(y(i), result(i))
        return result
    }

}
