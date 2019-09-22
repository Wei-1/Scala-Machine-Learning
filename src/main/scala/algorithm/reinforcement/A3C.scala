// Wei Chen - Asynchronous Advantage Actor Critic
// 2017-10-01

package com.scalaml.algorithm

// nextstate, reward, end = simulator(state, action)
class A3C(
    val actor_neurons: Array[Int],
    val critic_neurons: Array[Int],
    val initparas: Array[Double],
    val actnumber: Int,
    val simulator: (Array[Double], Int) => (Array[Double], Double, Boolean),
    val batchsize_number: Int = 100,
    val epsilon_saturation_number: Int = 10000,
    val train_number: Int = 10,
    val actor_learning_rate: Double = 0.01,
    val critic_learning_rate: Double = 0.01
) {

    val actor = new NeuralNetwork()
    actor.config(initparas.size +: actor_neurons :+ actnumber,
        _batchSize = batchsize_number, _gradientClipping = true)
    val critic = new NeuralNetwork()
    critic.config(initparas.size +: critic_neurons :+ 1,
        _batchSize = batchsize_number, _gradientClipping = true)

    val ex = new Exp

    def softmax(paras: Array[Double]): Array[Double] = {
        val tmpparas = paras.map(v => Math.exp(v))
        val parasum = tmpparas.sum
        tmpparas.map(_ / parasum)
    }

    class Exp {
        var c = 0
        var x = Array[Array[Double]]()
        var y = Array[Array[Double]]()
        var r = Array[Array[Double]]()
        def consume = {
            actor.train(x, y, iter = train_number, _learningRate = actor_learning_rate)
            critic.train(x, r, iter = train_number, _learningRate = critic_learning_rate)
            c = 0
            x = Array[Array[Double]]()
            y = Array[Array[Double]]()
            r = Array[Array[Double]]()
        }
        def add(paras: Array[Double], p_s: Array[Double], act: Int, reward: Double) {
            val advantage = reward - critic.predictOne(paras).head
            p_s(act) += advantage
            x :+= paras
            y :+= softmax(p_s)
            r :+= Array(reward)
            c += 1
            if (c >= batchsize_number) consume
        }
        def end = if (c > 0) consume
    }

    class ACState (val paras: Array[Double]) {
        def learn(df: Double, epoch: Int): Double = {
            val p_s = actor.predictOne(paras)
            val act = (if (scala.util.Random.nextDouble > epsilon) p_s.zipWithIndex.maxBy(_._1)._2
                else scala.util.Random.nextInt.abs % actnumber)
            if (epsilon > 0.1) epsilon -= depsilon
            val (newparas, newreward, newfinish) = simulator(paras, act)
            if (epoch > 0 && !newfinish) {
                val newstate = new ACState(newparas)
                val gradient = newreward + df * newstate.learn(df, epoch - 1) // max -> a: Q(s+1, a)
                ex.add(paras, p_s, act, gradient)
                gradient
            } else {
                ex.add(paras, p_s, act, newreward)
                newreward
            }
        }
        val bestAct: Int = actor.predictOne(paras).zipWithIndex.maxBy(_._1)._2
    }

    var epsilon = 1.0
    var depsilon = 0.9 / epsilon_saturation_number
    var state = new ACState(initparas)
    def train(number: Int = 1, df: Double = 0.6, epoch: Int = 100): Unit = {
        for (n <- 0 until number)
            state.learn(df, epoch)
        ex.end
    }
    def result(epoch: Int = 100): Array[ACState] = {
        var paras = initparas
        var curstate = new ACState(initparas)
        var arr: Array[ACState] = Array(curstate)
        var i = 0
        while (i < epoch) {
            i += 1
            val act = curstate.bestAct
            val (newparas, newreward, newfinish) = simulator(paras, act)
            if (newfinish) i = epoch
            paras = newparas
            curstate = new ACState(newparas)
            arr :+= curstate
        }
        arr
    }
}
