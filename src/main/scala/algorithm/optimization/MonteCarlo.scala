// Wei Chen - Monte Carlo
// 2017-08-08

package ght.mi.algorithm

class MCNode(val sim: Array[Double] => Double, val act: Array[Double] => Array[Array[Double]], val init: Array[Double], val parent: MCNode = null) {
    var score: Double = sim(init)
    var arr: Array[MCNode] = null // act(init).map(a => new MCNode(sim, act, a))
    var check: Boolean = false
    def best: MCNode = {
        if (check) {
            arr.maxBy(_.score).best
        } else {
            this
        }
    }
    def expand: Unit = {
        check = true
        arr = act(init).map(a => new MCNode(sim, act, a, this))
    }
    def backpropagate: Unit = {
        if (parent != null) {
            if (parent.score > score) {
                parent.score = score
                parent.backpropagate
            }
        }
    }
}

class MonteCarlo {
    def search(
        sim: Array[Double] => Double,
        act: Array[Double] => Array[Array[Double]],
        init: Array[Double],
        iter: Int
    ): MCNode = {
        val tree: MCNode = new MCNode(sim, act, init)
        for (i <- 0 until iter) {
            val node = tree.best
            node.backpropagate
            node.expand
        }
        tree.arr.maxBy(_.score)
    }
}
