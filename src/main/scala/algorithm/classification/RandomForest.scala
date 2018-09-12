// Wei Chen - Random Forest
// 2016-11-28

package com.interplanetarytech.algorithm

class RandomForest() extends Classifier {
    var trees = Array[DecisionTree]()
    var tree_n = 0 // Number of Trees
    var sample_n = 0 // Number of Sample Data in a Tree

    override def clear(): Boolean = try {
        trees = Array[DecisionTree]()
        tree_n = 0
        sample_n = 0
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    private def randomSelect(data: Array[(Int, Array[Double])], sample_n: Int) =
        scala.util.Random.shuffle(data.toList).take(sample_n).toArray

    private def addTree(data: Array[(Int, Array[Double])]) {
        val dtree = new DecisionTree()
        dtree.train(data)
        trees :+= dtree
    }

    override def config(paras: Map[String, Double]): Boolean = {
        tree_n = paras.getOrElse("TREE_NUMBER", paras.getOrElse("tree_number", paras.getOrElse("tree_n", 0)))
        sample_n = paras.getOrElse("SAMPLE_NUMBER", paras.getOrElse("sample_number", paras.getOrElse("sample_n", 0)))
    }

    override def train(data: Array[(Int, Array[Double])]): Boolean = try {
        val data_n = data.size
        if (data_n > sample_n) {
            for (i <- 0 until tree_n) addTree(randomSelect(data, sample_n))
        } else addTree(data)
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
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
