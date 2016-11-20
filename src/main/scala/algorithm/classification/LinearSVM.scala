// Wei Chen - LSVM - linear Support Vector Machine
// 2015-12-09

package ght.mi.algorithm
import ght.mi.algorithm.MatrixFunc._

// LinearSVM = Dual linear Support Vector Machine
// This core function only support dual classification cus linear
// classifier is only good at dual classification problems.
class LinearSVM() {

    var projector = Array[(Int, Int, Array[Double], Array[Double])]()
    def clear() = projector = Array[(Int, Int, Array[Double], Array[Double])]()

    // Sub Variables & Functions
    private val INF = 1.0/0             // Infinite
    private val rng = scala.util.Random // Random Seed
    private def dot(x:Array[Double], y:Array[Double]): Double =
        arraymultiply(x, y).sum

    // --- Function Core Start ---
    private def dualtrain(                      // Dual Linear SVM
        data: Array[(Int, Array[Double])],      // Data Array(yi, xi)
        cost: Map[Int, Double],                 // Cost of two groups
        limit: Int,                             // Iteration limit
        err: Double                             // Saturation error
    ): Array[Double] = {                        // Return weighting
        // - Feature Initialization
        val l = data.size                       // Data length
        val n = data.head._2.size               // Features numbers
        var w = new Array[Double](n)            // Initial weighting
        var alpha = new Array[Double](l)        // Alpha SV pointer
        var index = (0 until l).toArray         // Initialize index
        var QD = new Array[Double](l)           // QD // TODO
        val diag = cost.map(l => l._1 -> 0.5 / l._2)  // Diag
        for (i <- 0 until l) {
            val (yi, xi) = data(i)
            QD(i) = diag(yi) + xi.map(Math.pow(_,2)).sum // Initialize QD
        }
        // - Iteration Coefficients Initiation
        var saturated = false
        var iter = 0
        var PG_max_old =  INF // Projected Gradient maximum saved
        var PG_min_old = -INF // Projected Gradient minimum saved
        while (iter < limit && !saturated) {
            iter += 1
            var PG_max_new = -INF // Projected Gradient maximum new
            var PG_min_new =  INF // Projected Gradient minimum new
            for (i <- 0 until l) { // Randomize Saturation Direction
                var j = rng.nextInt % (l - i)
                if (j < 0) j += (l - i)
                val temp = index(i) // Random SWAP i <-> i+j
                index(i) = index(i+j)
                index(i+j) = temp
            }
            var outzone = false
            for (i <- index) { // Loop data with SWAP index
                val (yi, xi) = data(i)
                // Projected Gradient -> Cost with Alpha for G -> 0
                val G = yi * dot(w, xi) - 1 + alpha(i) * diag(yi)
                // if SV or Violate
                if (alpha(i) > 0 || G < 0) {
                    PG_max_new = Math.max(PG_max_new, G) // Sandwich Saturation
                    PG_min_new = Math.min(PG_min_new, G) // Test if all G ~= 0
                    val alpha_old = alpha(i)
                    alpha(i) = Math.max(alpha_old - G/QD(i), 0.0) // Update Alpha
                    val d = yi * (alpha(i) - alpha_old) // Difference
                    w = w.zip(xi).map(l => l._1 + l._2 * d) // wj += xij * d
                } else if (G <= PG_max_old) { // If in PG Zone
                    PG_max_new = Math.max(PG_max_new, 0.0) // Sandwich Saturation
                    PG_min_new = Math.min(PG_min_new, 0.0)
                } else outzone = true // Out of PG Zone
            }
            // Update valid PG Zone
            if (PG_max_new - PG_min_new > err) {
                PG_max_old = if (PG_max_new > 0) PG_max_new else  INF
                PG_min_old = if (PG_min_new < 0) PG_min_new else -INF
            } else if (outzone) { // Reset if PG not saturated correctly
                PG_max_old =  INF
                PG_min_old = -INF
            } else saturated = true // Done
        }
        // println("SVM iteration = " + iter)
        // println("Number of SV  = " + alpha.filter(_ > 0).size)
        return w
    }

    def train(
        data: Array[(Int, Array[Double])],      // Data Array(yi, xi)
        cost: Map[Int, Double],                 // Cost of two groups
        limit: Int,                             // Iteration limit
        err: Double                             // Saturation error
    ) = {
        val groupdata = data.groupBy(_._1).map(l => (l._1, l._2.map(_._2)))
        groupdata.map { group1 =>
            groupdata.map { group2 =>
                if (group1._1 < group2._1) {
                    val n = group1._2.size + group2._2.size
                    val m = matrixaccumulate(group1._2 ++ group2._2).map(_/n)
                    val w = dualtrain(
                        group1._2.map(g => (-1, arrayminus(g, m))) ++
                            group2._2.map(g => (1, arrayminus(g, m))),
                        cost,
                        limit,
                        err
                    )
                    projector :+= (group1._1, group2._1, m, w)
                }
            }
        }
    }

    // --- Dual Predict ---
    def predict(
        data: Array[Array[Double]]
    ): Array[Int] = {
        return data.map { d =>
            projector.map { p =>
                if (dot(arrayminus(d, p._3), p._4) < 0) p._1
                else p._2
            }.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
        }
    }
}
