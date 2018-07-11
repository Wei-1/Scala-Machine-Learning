// Wei Chen - LSVM - linear Support Vector Machine
// 2015-12-09

package ght.mi.algorithm
import ght.mi.general.MatrixFunc._

// LinearSVM = linear Support Vector Machine
// This core function only support dual classification cus linear
// classifier is only good at dual classification problems
class LinearSVM() {
    var projector = Array[Double]()
    def clear() = projector = Array[Double]()
    // --- Sub Variables & Functions ---
    private val INF = 1.0 / 0           // Infinite
    private val rng = scala.util.Random // Random Seed
    private def dot(x: Array[Double], y: Array[Double]): Double =
        arraymultiply(x, y).sum
    private def randomSwap(arr: Array[Int]) {
        val arrsize = arr.size
        for(i <- 0 until arrsize) { // Randomize Saturation Direction
            val j = rng.nextInt(arrsize - i)
            val temp = arr(i) // Random SWAP i <-> i + j
            arr(i) = arr(i + j)
            arr(i + j) = temp
        }
    }
    // --- Function Core Start ---
    def train(
        data: Array[(Int, Array[Double])], // Data Array(yi, xi)
        cost: Map[Int, Double],            // Cost of two groups
        limit: Int,                        // Iteration limit
        err: Double                        // Saturation error
    ) { // - Feature Initialization
        val traindatasize = data.size
        val featuresize = data.head._2.size
        var w = new Array[Double](featuresize + 1)   // Initial weighting
        var alpha = new Array[Double](traindatasize) // Alpha SV pointer
        var index = (0 until traindatasize).toArray  // Initialize index
        var QD = new Array[Double](traindatasize)    // QD // TODO
        val diag = cost.map(l => l._1 -> 0.5 / l._2) // Diag
        for(i <- 0 until traindatasize) {
            val (yi, xt) = data(i)
            val xi = xt :+ 1.0
            QD(i) = diag(yi) + xi.map(Math.pow(_, 2)).sum // Initialize QD
        }
        // - Iteration Coefficients Initiation
        var saturated = false
        var iter = 0
        var old_PG_max = INF // Projected Gradient maximum saved
        while(iter < limit && !saturated) {
            iter += 1
            var new_PG_max = -INF // new Projected Gradient maximum
            var new_PG_min =  INF // new Projected Gradient minimum
            randomSwap(index)
            var outzone = false
            for(i <- index) { // Loop data with SWAP index
                val (yi, xt) = data(i)
                val xi = xt :+ 1.0
                // Projected Gradient -> Cost with Alpha for PG -> 0
                val PG = yi * dot(w, xi) - 1 + alpha(i) * diag(yi)
                // if SV or Violate
                if(alpha(i) > 0 || PG < 0) {
                    new_PG_max = Math.max(new_PG_max, PG) // Sandwich Saturation
                    new_PG_min = Math.min(new_PG_min, PG) // Test if all PG ~= 0
                    val alpha_old = alpha(i)
                    alpha(i) = Math.max(alpha_old - PG / QD(i), 0.0) // Update Alpha
                    val d = yi * (alpha(i) - alpha_old) // Difference
                    w = w.zip(xi).map(l => l._1 + l._2 * d) // wj += xij * d
                } else if(PG <= old_PG_max) { // If in PG Zone
                    new_PG_max = Math.max(new_PG_max, 0.0) // Sandwich Saturation
                    new_PG_min = Math.min(new_PG_min, 0.0)
                } else outzone = true // Out of PG Zone
            }
            // Update valid PG Zone
            if(new_PG_max - new_PG_min > err) {
                old_PG_max = (if(new_PG_max > 0) new_PG_max else INF)
            } else if(outzone) { // Reset if PG not saturated correctly
                old_PG_max = INF
            } else saturated = true // Done
        }
        // Console.err.println("Iterate:" + iter + "  SV_Number:" + alpha.filter(_ > 0).size)
        // Console.err.println("W = " + w.mkString(","))
        projector = w
    }
    // --- Prediction Function ---
    def predict(data: Array[Array[Double]]): Array[Int] = {
        data.map { xt =>
            if(dot(xt :+ 1.0, projector) < 0) -1 else 1
        }
    }
}
