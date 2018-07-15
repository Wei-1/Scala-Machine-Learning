// Wei Chen - Q-Learning
// 2017-08-16

package com.interplanetarytech.algorithm

class QState (val id: Int = -1) {
    var reward: Double = 0.0
    var links: Map[QState, Double] = Map[QState, Double]() // action, Q
    def setLinks(newlinks: Map[QState, Double]): Unit = links ++= newlinks
    def best: QState = links.maxBy(_._2)._1
    def setReward(newreward: Double): Unit = reward = newreward
    def learn(lr: Double, df: Double, epoch: Int): Double = {
        if (epoch > 0) {
            links = links.map { case (s, q_sa) =>
                val gradient = reward + df * s.learn(lr, df, epoch - 1) // max -> a: Q(s+1, a)
                val n_q_sa = (1 - lr) * q_sa + lr * gradient
                (s, n_q_sa)
            }
        }
        if (links.size > 0)
            links.maxBy(_._2)._2 // max -> a: Q(s, a)
        else
            reward
    }
}

class QLearning(val statenumber: Int = 0) {
    val states = (0 until statenumber).map(id => new QState(id)).toArray
    def addRewards(rewards: Map[Int, Double]): Unit = {
        rewards.map { case (id, reward) =>
            states(id).setReward(reward)
        }
    }
    def addLinks(links: Map[Int, Array[Int]]): Unit = {
        links.map { case (id, linkids) =>
            states(id).setLinks(linkids.map(linkid => (states(linkid), 0.0)).toMap)
        }
    }
    def iterate(number: Int = 1, lr: Double = 0.1, df: Double = 0.6, epoch: Int = 100): Unit = {
        for (n <- 0 until number)
            states(0).learn(lr, df, epoch)
    }
    def result(epoch: Int = 100): Array[QState] = {
        var curstate = states(0)
        var arr: Array[QState] = Array(curstate)
        var i = 0
        while (i < epoch && curstate.links.size > 0) {
            i += 1
            curstate = curstate.best
            arr :+= curstate
        }
        arr
    }
}
