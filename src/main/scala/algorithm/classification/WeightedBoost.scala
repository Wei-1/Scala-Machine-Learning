// Wei Chen - Weighted Boost
// 2018-09-26

package com.scalaml.algorithm

class WeightedBoost() extends Classification {
    val algoname: String = "WeightedBoost"
    val version: String = "0.1"

    var classifiers = Array[Classification]()
    var weight = Array[Double]()

    override def clear(): Boolean = {
        classifiers = Array[Classification]()
        weight = Array[Double]()
        true
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        classifiers = paras.getOrElse("CLASSIFIERS", paras.getOrElse("classifiers", Array(new BayesianDecision): Any)).asInstanceOf[Array[Classification]]
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def train(data: Array[(Int, Array[Double])]): Boolean = {
        if(classifiers.forall(classifier => classifier.train(data))) {
            weight = classifiers.map(classifier => classifier.predict(data.map(_._2)).zip(data.map(_._1)).count(p => p._1 == p._2) / data.size.toDouble)
            true
        } else false
    }

    override def predict(data: Array[Array[Double]]): Array[Int] = {
        val results = classifiers.map(classifier => classifier.predict(data))
        (for(i <- 0 until data.size) yield {
            var weightmap = Map[Int, Double]()
            results.map(_(i)).zip(weight).foreach { case (k, w) => weightmap += k -> (weightmap.getOrElse(k, 0.0) + w) }
            weightmap.maxBy(_._2)._1
        }).toArray
    }
}
