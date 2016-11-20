// Wei Chen - Restricted Boltzmann Machine
// 2016-11-20

package ght.mi.algorithm
import ght.mi.algorithm.MatrixFunc._

class RBM(val visible_n: Int, val hidden_n: Int) {

    val a: Double = 1.0 / visible_n
    var syns: Array[Array[Double]] = matrixrandom(hidden_n, visible_n, -a, a)
    var hbias: Array[Double] = new Array[Double](hidden_n)
    var vbias: Array[Double] = new Array[Double](visible_n)

    def sigmoid(x: Double): Double = 1 / (1 + Math.exp(-x))

    def clear() {
        syns = matrixrandom(hidden_n, visible_n, -a, a)
        hbias = new Array[Double](hidden_n)
        vbias = new Array[Double](visible_n)
    }
    
    def train(v_input: Array[Double], lr: Double, k: Int) {
        val ph_samples = new Array[Double](hidden_n)
        val nv_samples = new Array[Double](visible_n)
        val nh_samples = new Array[Double](hidden_n)

        // Contrastive Divergence (CD) - k
        construct(v_input, ph_samples)
        for (i <- 0 until k) {
            if (i == 0) reconstruct(ph_samples, nv_samples)
            else reconstruct(nh_samples, nv_samples)
            construct(nv_samples, nh_samples)
        }

        for (i <- 0 until hidden_n) {
            for (j <- 0 until visible_n) {
                syns(i)(j) += lr * (ph_samples(i) * v_input(j) - nh_samples(i) * nv_samples(j))
            }
            hbias(i) += lr * (ph_samples(i) - nh_samples(i))
        }

        for (i <- 0 until visible_n) {
            vbias(i) += lr * (v_input(i) - nv_samples(i))
        }
    }

    def propup(v: Array[Double], j: Int, b: Double): Double = {
        var temp: Double = b
        for (i <- 0 until visible_n) temp += syns(j)(i) * v(i)
        sigmoid(temp)
    }

    def propdown(h: Array[Double], i: Int, b: Double): Double = {
        var temp: Double = b
        for (j <- 0 until hidden_n) temp += syns(j)(i) * h(j)
        sigmoid(temp)
    }

    def construct(v_sample: Array[Double], h_sample: Array[Double]) { // v to h
        for (j <- 0 until hidden_n) {
            h_sample(j) = propup(v_sample, j, hbias(j))
        }
    }

    def reconstruct(h_sample: Array[Double], v_sample: Array[Double]) { // h to v
        for (i <- 0 until visible_n) {
            v_sample(i) = propdown(h_sample, i, vbias(i))
        }
    }
}