// Wei Chen - Regression Tree
// 2022-12-28

package com.scalaml.algorithm

class RegressionNode(
    val col: Int, val v: Double,
    val tnode: RegressionNode, val fnode: RegressionNode,
    val r: Double = 0,
    val cats: Set[Int] = Set[Int]()
) {
    def predict(x: Array[Double]): Double = {
        if(tnode != null && fnode != null) {
            if((!cats.contains(col) && x(col) > v) || x(col) == v) tnode.predict(x)
            else fnode.predict(x)
        } else r
    }
    override def toString: String = {
        if(tnode != null && fnode != null) {
            s"col[$col]" + (if(cats.contains(col)) " == " else " >= ") + v +
            s" ? ($tnode) : ($fnode)"
        } else s"class[$r]"
    }
}

class RegressionTree() extends Regression {
    val algoname: String = "RegressionTree"
    val version: String = "0.1"

    var tree: RegressionNode = null
    var catColumns: Set[Int] = Set[Int]()
    var maxLayer: Int = 5

    override def clear(): Boolean = {
        tree = null
        true
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        catColumns = paras.getOrElse("CATEGORYCOLUMNS", paras.getOrElse("catColumns", Set[Int]())).asInstanceOf[Set[Int]]
        maxLayer = paras.getOrElse("maxLayer", 5.0).asInstanceOf[Double].toInt
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    private def log2(x: Double) = Math.log(x) / Math.log(2)

    private def entropy(data: Array[(Double, Array[Double])]): Double = {
        val dataSize = data.size.toDouble
        val dataAvg = data.map(_._1).sum / dataSize
        data.map(d => Math.abs(d._1 - dataAvg)).sum / dataSize
    }

    private def buildtree(data: Array[(Double, Array[Double])], layer: Int = maxLayer): RegressionNode = {
        var currentScore: Double = entropy(data)
        var bestGain: Double = 0
        var bestColumn: Int = 0
        var bestValue: Double = 0
        var bestTrueData = Array[(Double, Array[Double])]()
        var bestFalseData = Array[(Double, Array[Double])]()

        val dataSize = data.size.toDouble
        val columnSize: Int = data.head._2.size
        for (col <- 0 until columnSize) {
            var valueSet: Set[Double] = Set()
            for (d <- data) valueSet += d._2(col)
            for (value <- valueSet) {
                val (tData, fData) = data.partition { d =>
                    if(catColumns.contains(col)) d._2(col) == value
                    else d._2(col) >= value
                }
                val p = tData.size / dataSize
                val gain = currentScore - p * entropy(tData) - (1 - p) * entropy(fData)
                if (gain > bestGain && tData.size > 0 && fData.size > 0) {
                    bestGain = gain
                    bestColumn = col
                    bestValue = value
                    bestTrueData = tData
                    bestFalseData = fData
                }
            }
        }
        if (bestGain > 0 && layer > 0) {
            val tnode = buildtree(bestTrueData, layer - 1)
            val fnode = buildtree(bestFalseData, layer - 1)
            new RegressionNode(bestColumn, bestValue, tnode, fnode)
        } else new RegressionNode(0, 0, null, null, data.map(_._1).sum / dataSize)
    }

    override def train(data: Array[(Double, Array[Double])]): Boolean = try {
        tree = buildtree(data)
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def predict(x: Array[Array[Double]]): Array[Double] = x.map(xi => tree.predict(xi))
}
