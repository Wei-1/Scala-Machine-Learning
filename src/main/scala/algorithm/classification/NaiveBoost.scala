// Wei Chen - Naive Boost
// 2018-09-26

package com.scalaml.algorithm

class NaiveBoost() extends Classification {
    val algoname: String = "NaiveBoost"
    val version: String = "0.1"

    var classifiers = Array[Classification]()

    override def clear(): Boolean = {
        classifiers = Array[Classification]()
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
        classifiers.forall(classifier => classifier.train(data))
    }

    override def predict(data: Array[Array[Double]]): Array[Int] = {
        val results = classifiers.map(classifier => classifier.predict(data))
        (for(i <- 0 until data.size) yield {
            results.map(_(i)).groupBy(identity).maxBy(_._2.size)._1
        }).toArray
    }
}
