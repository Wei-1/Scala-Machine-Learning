// Wei Chen - Prioritized Experience Replay (PER)
// 2017-08-31

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

// nextstate, reward, end = simulator(state, action)
class PER(
    val layer_neurons: Array[Int],
    val initparas: Array[Double],
    val actnumber: Int,
    val simulator: (Array[Double], Int) => (Array[Double], Double, Boolean),
    val batchsize_number: Int = 100,
    val epsilon_saturation_number: Int = 10000,
    val train_number: Int = 10,
    val nn_learning_rate: Double = 0.01,
    val prior_eps: Double = 1e-6,
    val alpha: Double = 0.6,
    var beta: Double = 0.6
) {

    val nn = new NeuralNetwork()
    nn.config(initparas.size +: layer_neurons :+ actnumber,
        _batchSize = batchsize_number, _gradientClipping = true)
    val ex = new Exp

    class Exp {
        var c = 0
        var x = Array[Array[Double]]()
        var y = Array[Array[Double]]()
        var max_priority = 1.0
        var fin_priority = Array[Double]()

        def consume = {
            val indices = _sample_proportional()
            var nx = Array[Array[Double]]()
            var ny = Array[Array[Double]]()
            var nw = Array[Double]()
            for (i <- indices) {
                nx :+= x(i)
                ny :+= y(i)
                nw :+= _calculate_weight(i, beta)
            }
            val nyp = nn.predict(nx)
            val new_priorities = ny.zip(nyp).map { case (nyi, nypi) =>
                math.sqrt(arrayminussquare(nyi, nypi).sum) + prior_eps
            }

            nn.train(
                nx, ny,
                iter = train_number,
                _learningRate = nn_learning_rate,
                _outputWeights = nw
            )

            // _update_priorities(indices, new_priorities)
            for (priority <- new_priorities) {
                max_priority = math.max(max_priority, priority)
            }
            x = Array[Array[Double]]()
            y = Array[Array[Double]]()
            fin_priority = Array[Double]()
            c = 0
        }
        def add(paras: Array[Double], target: Array[Double]) {
            x :+= paras
            y :+= target
            fin_priority :+= math.pow(max_priority, alpha)
            c += 1
            if (c >= batchsize_number) consume
        }
        def end = if (c > 0) consume
        // Functions for PER
        def _sample_proportional(): Array[Int] = {
            // Sample indices based on proportions
            val indices = new Array[Int](batchsize_number)
            val p_sum = fin_priority.sum
            val segment = p_sum / batchsize_number
            for (i <- 0 until batchsize_number) {
                val a = segment * i
                val b = segment * (i + 1)
                val upperbound = scala.util.Random.nextDouble * (b - a) + a
                val idx = _retrieve(upperbound)
                indices(i) = idx
            }
            indices
        }
        def _retrieve(upperbound: Double): Int = {
            var a = 0.0
            var i = 0
            while (a < upperbound) {
                a += fin_priority(i)
                i += 1
            }
            i - 1
        }
        def _calculate_weight(idx: Int, beta: Double): Double = {
            // Calculate the weight of the experience at idx
            // get max weight
            val p_sum = fin_priority.sum
            val p_min = fin_priority.min / p_sum
            val max_weight = math.pow(p_min * c, -beta)
            // calculate weights
            val p_sample = fin_priority(idx) / p_sum
            val weight = math.pow(p_sample * c, -beta)
            weight / max_weight
        }
        // def _update_priorities(indices: Array[Int], priorities: Array[Double]) {
        //     // Update priorities of sampled transitions
        //     for ((idx, priority) <- indices.zip(priorities)) {
        //         fin_priority(idx) = math.pow(priority, alpha)
        //         max_priority = math.max(max_priority, priority)
        //     }
        // }
    }

    class DQState (val paras: Array[Double]) {
        def learn(lr: Double, df: Double, epoch: Int): Double = {
            val q_s = nn.predictOne(paras)
            val act = (if (scala.util.Random.nextDouble > epsilon) q_s.zipWithIndex.maxBy(_._1)._2
                else scala.util.Random.nextInt.abs % actnumber)
            if (epsilon > 0.1) epsilon -= depsilon
            val (newparas, newreward, newfinish) = simulator(paras, act)
            if (epoch > 0 && !newfinish) {
                val newstate = new DQState(newparas)
                val gradient = newreward + df * newstate.learn(lr, df, epoch - 1) // max -> a: Q(s+1, a)
                q_s(act) = (1 - lr) * q_s(act) + lr * gradient
            } else {
                q_s(act) = newreward
            }
            ex.add(paras, q_s) // nn.train(Array(paras), Array(q_s), batchsize_number, lr)
            q_s.max
        }
        val bestAct: Int = nn.predictOne(paras).zipWithIndex.maxBy(_._1)._2
    }

    var epsilon = 1.0
    var depsilon = 0.9 / epsilon_saturation_number
    var state = new DQState(initparas)
    def train(number: Int = 1, lr: Double = 0.1, df: Double = 0.6, epoch: Int = 100): Unit = {
        for (n <- 0 until number) {
            state.learn(lr, df, epoch)
            val fraction = math.min(n / number, 1.0)
            beta += fraction * (1.0 - beta)
        }
        ex.end
    }
    def result(epoch: Int = 100): Array[DQState] = {
        var paras = initparas
        var curstate = new DQState(initparas)
        var arr: Array[DQState] = Array(curstate)
        var i = 0
        while (i < epoch) {
            i += 1
            val act = curstate.bestAct
            val (newparas, newreward, newfinish) = simulator(paras, act)
            if (newfinish) i = epoch
            paras = newparas
            curstate = new DQState(newparas)
            arr :+= curstate
        }
        arr
    }
}
