// Wei Chen - Dueling Deep Q Network
// 2017-10-07

package com.scalaml.algorithm

// nextstate, reward, end = simulator(state, action)
class DDQN(
    val layer_neurons: Array[Int],
    val initparas: Array[Double],
    val actnumber: Int,
    val simulator: (Array[Double], Int) => (Array[Double], Double, Boolean),
    val batchsize_number: Int = 100,
    val epsilon_saturation_number: Int = 10000,
    val train_number: Int = 10,
    val nn_learning_rate: Double = 0.01
) {

    val nn1 = new NeuralNetwork()
    nn1.config(initparas.size +: layer_neurons :+ actnumber,
        _batchSize = batchsize_number, _gradientClipping = true)
    val nn2 = new NeuralNetwork()
    nn2.config(initparas.size +: layer_neurons :+ actnumber,
        _batchSize = batchsize_number, _gradientClipping = true)
    val ex1 = new Exp(nn2)
    val ex2 = new Exp(nn1)

    class Exp(nn: NeuralNetwork) {
        var c = 0
        var x = Array[Array[Double]]()
        var y = Array[Array[Double]]()
        def consume = {
            nn.train(x, y, iter = train_number, _learningRate = nn_learning_rate)
            c = 0
            x = Array[Array[Double]]()
            y = Array[Array[Double]]()
        }
        def add(paras: Array[Double], target: Array[Double]) {
            x :+= paras
            y :+= target
            c += 1
            if (c >= batchsize_number) consume
        }
        def end = if (c > 0) consume
    }

    class DQState (val paras: Array[Double]) {
        def learn(lr: Double, df: Double, epoch: Int): Double = {
            val q_s1 = nn1.predictOne(paras)
            val q_s2 = nn2.predictOne(paras)
            val act1 = (if (scala.util.Random.nextDouble > epsilon) q_s1.zipWithIndex.maxBy(_._1)._2
                else scala.util.Random.nextInt.abs % actnumber)
            val act2 = (if (scala.util.Random.nextDouble > epsilon) q_s2.zipWithIndex.maxBy(_._1)._2
                else scala.util.Random.nextInt.abs % actnumber)
            if (epsilon > 0.1) epsilon -= depsilon
            val (newparas1, newreward1, newfinish1) = simulator(paras, act1)
            val (newparas2, newreward2, newfinish2) = simulator(paras, act1)
            if (epoch > 0 && !newfinish1) {
                val gradient = newreward1 + df * new DQState(newparas1).learn(lr, df, epoch - 1) // max -> a: Q(s+1, a)
                q_s1(act1) = (1 - lr) * q_s1(act1) + lr * gradient
            } else {
                q_s1(act1) = newreward1
            }
            if (epoch > 0 && !newfinish2) {
                val gradient = newreward2 + df * new DQState(newparas2).learn(lr, df, epoch - 1) // max -> a: Q(s+1, a)
                q_s2(act2) = (1 - lr) * q_s2(act2) + lr * gradient
            } else {
                q_s2(act2) = newreward2
            }
            ex1.add(paras, q_s2) // nn.train(Array(paras), Array(q_s), batchsize_number, lr)
            ex2.add(paras, q_s1) // nn.train(Array(paras), Array(q_s), batchsize_number, lr)
            (q_s1.max + q_s2.max) / 2
        }
        val bestAct: Int = nn1.predictOne(paras).zipWithIndex.maxBy(_._1)._2
    }

    var epsilon = 1.0
    var depsilon = 0.9 / epsilon_saturation_number
    var state = new DQState(initparas)
    def train(number: Int = 1, lr: Double = 0.1, df: Double = 0.6, epoch: Int = 100): Unit = {
        for (n <- 0 until number)
            state.learn(lr, df, epoch)
        ex1.end
        ex2.end
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
