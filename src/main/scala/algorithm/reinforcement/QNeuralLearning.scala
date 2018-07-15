// Wei Chen - Q Neural Learning
// 2017-08-16

package com.interplanetarytech.algorithm

// nextstate, reward, end = simulator(state, action)
class QNeuralLearning(
    val layer_neurons: Array[Int],
    val initparas: Array[Double],
    val actnumber: Int,
    val simulator: (Array[Double], Int) => (Array[Double], Double, Boolean),
    val batchsize_number: Int = 100,
    val epsilon_saturation_number: Int = 10000,
    val nn_learning_rate: Double = 0.01
) {

    val nn = new NeuralNetwork(layer_neurons, initparas.size, actnumber)

    class QNState (val paras: Array[Double]) {
        def learn(lr: Double, df: Double, epoch: Int): Double = {
            val q_s = nn.predict(Array(paras)).head
            val act = (if (scala.util.Random.nextDouble > epsilon) q_s.zipWithIndex.maxBy(_._1)._2
                else scala.util.Random.nextInt.abs % actnumber)
            if (epsilon > 0.1) epsilon -= depsilon
            val (newparas, newreward, newfinish) = simulator(paras, act)
            if (epoch > 0 && !newfinish) {
                val newstate = new QNState(newparas)
                val gradient = newreward + df * newstate.learn(lr, df, epoch - 1) // max -> a: Q(s+1, a)
                q_s(act) = (1 - lr) * q_s(act) + lr * gradient
            } else {
                q_s(act) = newreward
            }
            nn.train(Array(paras), Array(q_s), batchsize_number, nn_learning_rate)
            q_s.max
        }
        val bestAct: Int = nn.predict(Array(paras)).head.zipWithIndex.maxBy(_._1)._2
    }

    var epsilon = 1.0
    var depsilon = 0.9 / epsilon_saturation_number
    var state = new QNState(initparas)
    def train(number: Int = 1, lr: Double = 0.1, df: Double = 0.6, epoch: Int = 100): Unit = {
        for (n <- 0 until number)
            state.learn(lr, df, epoch)
    }
    def result(epoch: Int = 100): Array[QNState] = {
        var paras = initparas
        var curstate = new QNState(initparas)
        var arr: Array[QNState] = Array(curstate)
        var i = 0
        while (i < epoch) {
            i += 1
            val act = curstate.bestAct
            val (newparas, newreward, newfinish) = simulator(paras, act)
            if (newfinish) i = epoch
            paras = newparas
            curstate = new QNState(newparas)
            arr :+= curstate
        }
        arr
    }
}
