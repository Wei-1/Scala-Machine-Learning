// Wei Chen - Random Cut Forest
// 2022-03-05

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class RandomCutForest() extends Abnormal {
    val algoname: String = "RandomCutForest"
    val version: String = "0.1"

    var trees = Array[RandomCutTree]()
    var tree_n = 10 // Number of Trees
    var sample_n = 10 // Number of Sample Data in a Tree
    var maxLayer = 5

    override def clear(): Boolean = {
        trees = Array[RandomCutTree]()
        tree_n = 10 // Number of Trees
        sample_n = 10 // Number of Sample Data in a Tree
        maxLayer = 5
        true
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        tree_n = paras.getOrElse("TREE_NUMBER", paras.getOrElse("tree_number", paras.getOrElse("tree_n", 10.0))).asInstanceOf[Double].toInt
        sample_n = paras.getOrElse("SAMPLE_NUMBER", paras.getOrElse("sample_number", paras.getOrElse("sample_n", 10.0))).asInstanceOf[Double].toInt
        maxLayer = paras.getOrElse("maxLayer", 5.0).asInstanceOf[Double].toInt
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    private def randomSelect(data: Array[Array[Double]], sample_n: Int) =
        scala.util.Random.shuffle(data.toList).take(sample_n).toArray

    private def addTree(data: Array[Array[Double]]): Boolean = {
        val itree = new RandomCutTree()
        var paras = Map("maxLayer" -> maxLayer.toDouble): Map[String, Any]
        val check = itree.config(paras) && itree.train(data)
        if(check) trees :+= itree
        check
    }

    override def train(data: Array[Array[Double]]): Boolean = {
        val data_n = data.size
        if (data_n > sample_n) {
            (0 until tree_n).forall(i => addTree(randomSelect(data, sample_n)))
        } else addTree(data)
    }

    override def predict(data: Array[Array[Double]]): Array[Double] = {
        matrixaccumulate(trees.map { tree =>
            tree.predict(data)
        }).map(_ / tree_n)
    }
}
