// Wei Chen - Isolation Tree
// 2022-03-04

package com.scalaml.algorithm

class IsolationTree() extends Abnormal {
    val algoname: String = "IsolationTree"
    val version: String = "0.1"

    var maxLayer = 5
    var tree: DecisionNode = null

    override def clear(): Boolean = {
        maxLayer = 5
        true
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        maxLayer = paras.getOrElse("maxLayer", 5.0).asInstanceOf[Double].toInt
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    private def buildtree(data: Array[Array[Double]], layer: Int = 0): DecisionNode = {
        val dataSize = data.size
        val columnSize: Int = data.head.size
        val col = scala.util.Random.nextInt(columnSize)
        val colData = data.map(d => d(col))
        val minV = colData.min
        val maxV = colData.max
        val value = (maxV - minV) * scala.util.Random.nextDouble() + minV
        val (tData, fData) = data.partition { d =>
            d(col) >= value
        }
        if (tData.size > 0 && fData.size > 0 && layer < maxLayer) {
            val tnode = buildtree(tData, layer + 1)
            val fnode = buildtree(fData, layer + 1)
            new DecisionNode(col, value, tnode, fnode)
        } else new DecisionNode(0, 0, null, null, layer)
    }

    override def train(data: Array[Array[Double]]): Boolean = try {
        tree = buildtree(data)
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def predict(x: Array[Array[Double]]): Array[Double] = x.map(xi => tree.predict(xi))
}