// Wei Chen - Naive Feedback Learning
// 2017-07-28

package com.scalaml.algorithm

class FState (val id: Int = -1) {
    var score: Double = 0.0
    var links: Map[FState, Double] = Map[FState, Double]()
    def setLinks(newlinks: Map[FState, Double]): Unit = links ++= newlinks
    def best: FState = links.maxBy(_._2)._1
    def explore(scale: Int = 3): FState = {
        val linksize = links.size
        val varr = links.toArray.map(_._2)
        val vavg = varr.sum / linksize
        val vstd = varr.map(v => Math.pow(v - vavg, 2)).sum / linksize
        val vmin = varr.min
        links.maxBy { case (_, v) => (v - vmin + vstd * scale + scale) * scala.util.Random.nextDouble }._1
    }
    def setScore(newscore: Double): Unit = score = newscore
    def feedback(ns: FState, lr: Double): Unit = {
        val gradient = (ns.score - score) * lr
        links += ns -> (links(ns) + gradient)
        score += gradient
    }
}

class NaiveFeedback(val statenumber: Int = 0) {
    val states = (0 until statenumber).map(id => new FState(id)).toArray
    def addScores(scores: Map[Int, Double]): Unit = {
        scores.map { case (id, score) =>
            states(id).setScore(score)
        }
    }
    def addLinks(links: Map[Int, Array[Int]]): Unit = {
        links.map { case (id, linkids) =>
            states(id).setLinks(linkids.map(linkid => (states(linkid), 0.0)).toMap)
        }
    }
    def iterate(number: Int = 1, lr: Double = 0.1, scale: Int = 3, epoch: Int = 100): Unit = {
        for (n <- 0 until number) {
            var curstate = states(0)
            var arr: Array[FState] = Array(curstate)
            var i = 0
            while (i < epoch && curstate.links.size > 0) {
                i += 1
                curstate = curstate.explore(scale)
                arr :+= curstate
            }
            for (i <- arr.size - 1 to 1 by -1) {
                arr(i-1).feedback(arr(i), lr)
            }
            // Console.err.println(arr.map(_.id).mkString(","))
        }
    }
    def result(epoch: Int = 100): Array[FState] = {
        var curstate = states(0)
        var arr: Array[FState] = Array(curstate)
        var i = 0
        while (i < epoch && curstate.links.size > 0) {
            i += 1
            curstate = curstate.best
            arr :+= curstate
        }
        arr
    }
}
