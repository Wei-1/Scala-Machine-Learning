// Wei Chen - Decision Tree
// 2016-11-24

package com.scalaml.algorithm

class DecisionNode(
    val col: Int, val v: Double,
    val tnode: DecisionNode, val fnode: DecisionNode,
    val r: Map[Int, Int],
    val cats: Set[Int] = Set[Int]()
) {
    def predict(x: Array[Double]): Int = {
        if(r == null) {
            if((!cats.contains(col) && x(col) > v) || x(col) == v) tnode.predict(x)
            else fnode.predict(x)
        } else r.maxBy(_._2)._1
    }
    override def toString: String =
        col + (if(cats.contains(col)) " == " else " >= ") + v + " ? " +
        tnode + " : " + fnode
}

class DecisionTree() extends Classification {
    val algoname: String = "DecisionTree"
    val version: String = "0.1"

    var tree: DecisionNode = null
    var catColumns: Set[Int] = Set[Int]()
    var maxLayer: Int = 5

    override def clear(): Boolean = try {
        tree = null
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
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

    private def uniqueCount(data: Array[(Int, Array[Double])]): Map[Int, Int] =
        data.groupBy(_._1).map(t => (t._1, t._2.size))

    private def entropy(data: Array[(Int, Array[Double])]): Double = {
        val dataSize = data.size.toDouble
        uniqueCount(data).map { case (k, v) =>
            val p = v / dataSize
            -p * log2(p)
        }.sum
    }

    private def buildtree(data: Array[(Int, Array[Double])], layer: Int = maxLayer): DecisionNode = {
        var currentScore: Double = entropy(data)
        var bestGain: Double = 0
        var bestColumn: Int = 0
        var bestValue: Double = 0
        var bestTrueData = Array[(Int, Array[Double])]()
        var bestFalseData = Array[(Int, Array[Double])]()

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
                val gain = currentScore - p * entropy(tData) - (1-p) * entropy(fData)
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
            new DecisionNode(bestColumn, bestValue, tnode, fnode, null)
        } else new DecisionNode(0, 0, null, null, uniqueCount(data))
    }

    override def train(data: Array[(Int, Array[Double])]): Boolean = try {
        tree = buildtree(data)
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def predict(x: Array[Array[Double]]): Array[Int] = x.map(xi => tree.predict(xi))
}
