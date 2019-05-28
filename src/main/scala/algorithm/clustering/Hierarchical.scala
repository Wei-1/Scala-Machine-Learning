// Wei Chen - Hierarchical
// 2016-11-12

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class Hierarchical() extends Clustering {
    val algoname: String = "Hierarchical"
    val version: String = "0.1"

    var splittree = Array[(Int, Int)]()
    var treecheck = Map[Int, Int]()
    var group = 2

    def distArr(a1: Array[Double], a2: Array[Double]): Double =
        Math.sqrt(arrayminussquare(a1, a2).sum)
    def cascade(p: Int, c: Int) {
        splittree.map{ l =>
            if (l._1 == p && treecheck(l._2) < 0) {
                treecheck += (l._2 -> c)
                cascade(l._2, c)
            } else if (l._2 == p && treecheck(l._1) < 0) {
                treecheck += (l._1 -> c)
                cascade(l._1, c)
            }
        }
    }

    override def clear(): Boolean = try {
        splittree = Array[(Int, Int)]()
        treecheck = Map[Int, Int]()
        group = 2
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        group = paras.getOrElse("GROUP", paras.getOrElse("group", 2)).asInstanceOf[Int]
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }
    // --- Hierarchical ---
    override def cluster(          // Hierarchical
        data: Array[Array[Double]] // Data Array(xi)
    ): Array[Int] = {
        val n = data.size
        var undone = Map(0 -> 0.0)
        undone ++= (1 until n).map((_, Double.MaxValue))
        var tree = Map[Int, (Int, Double)]()
        while (!undone.isEmpty) {
            val node = undone.minBy(_._2)._1
            undone -= node
            for (i <- 0 until n) {
                if (i != node && undone.contains(i)) {
                    val v = distArr(data(node), data(i))
                    if (v < undone(i)) {
                        undone += (i -> v)
                        tree += (i -> (node, v))
                    }
                }
            }
        }
        splittree = tree.toArray.sortBy(_._2._2).dropRight(group-1).map(l => (l._1, l._2._1))
        treecheck = (0 until n).map((_, -1)).toMap
        var c = 1
        for (i <- 0 until n) {
            if (treecheck(i) < 0) {
                treecheck += (i -> c)
                cascade(i, c)
                c += 1
            }
        }
        return treecheck.toArray.sortBy(_._1).map(_._2)
    }
}
