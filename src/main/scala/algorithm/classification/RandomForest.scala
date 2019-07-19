// Wei Chen - Random Forest
// 2016-11-28

package com.scalaml.algorithm

class RandomForest() extends Classification {
    val algoname: String = "RandomForest"
    val version: String = "0.1"

    var trees = Array[DecisionTree]()
    var tree_n = 10 // Number of Trees
    var sample_n = 10 // Number of Sample Data in a Tree
    var catColumns = Set[Int]()
    var maxLayer = 5

    override def clear(): Boolean = {
        trees = Array[DecisionTree]()
        tree_n = 10
        sample_n = 10
        true
    }

    private def randomSelect(data: Array[(Int, Array[Double])], sample_n: Int) =
        scala.util.Random.shuffle(data.toList).take(sample_n).toArray

    private def addTree(data: Array[(Int, Array[Double])]): Boolean = {
        val dtree = new DecisionTree()
        var paras = Map("maxLayer" -> maxLayer.toDouble): Map[String, Any]
        if(catColumns.size > 0) paras += "catColumns" -> catColumns
        val check = dtree.config(paras) && dtree.train(data)
        if(check) trees :+= dtree
        check
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        tree_n = paras.getOrElse("TREE_NUMBER", paras.getOrElse("tree_number", paras.getOrElse("tree_n", 10.0))).asInstanceOf[Double].toInt
        sample_n = paras.getOrElse("SAMPLE_NUMBER", paras.getOrElse("sample_number", paras.getOrElse("sample_n", 10.0))).asInstanceOf[Double].toInt
        catColumns = paras.getOrElse("CATEGORYCOLUMNS", paras.getOrElse("catColumns", Set[Int]())).asInstanceOf[Set[Int]]
        maxLayer = paras.getOrElse("maxLayer", 5.0).asInstanceOf[Double].toInt
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def train(data: Array[(Int, Array[Double])]): Boolean = {
        val data_n = data.size
        if (data_n > sample_n) {
            (0 until tree_n).forall(i => addTree(randomSelect(data, sample_n)))
        } else addTree(data)
    }

    override def predict(data: Array[Array[Double]]): Array[Int] = {
        val data_n = data.size
        return trees.map { tree =>
            tree.predict(data)
        }.foldLeft(Array.fill(data_n)(Map[Int, Int]())) { case (a, b) =>
            a.zip(b).map { case l =>
                var c: Map[Int, Int] = l._1
                val v: Int = l._2
                if (c.contains(v)) {
                    c += (v -> (c(v) + 1))
                    c
                } else c + (v -> 1)
            }
        }.map(_.maxBy(_._2)._1)
    }
}
