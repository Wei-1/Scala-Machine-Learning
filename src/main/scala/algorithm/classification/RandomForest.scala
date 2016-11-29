// Wei Chen - Random Forest
// 2016-11-28

package ght.mi.algorithm

class RandomForest() {
    var trees = Array[DecisionTree]()
    def clear() = trees = Array[DecisionTree]()

    private def randomSelect(data: Array[(Int, Array[Double])], sample_n: Int) =
        scala.util.Random.shuffle(data.toList).take(sample_n).toArray

    private def addTree(data: Array[(Int, Array[Double])]) {
        val dtree = new DecisionTree()
        dtree.train(data)
        trees :+= dtree
    }

    def train(data: Array[(Int, Array[Double])], tree_n: Int, sample_n: Int) {
        val data_n = data.size
        if (data_n > sample_n) {
            for (i <- 0 until tree_n) addTree(randomSelect(data, sample_n))
        } else addTree(data)
    }

    def predict(data: Array[Array[Double]]): Array[Int] = {
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
