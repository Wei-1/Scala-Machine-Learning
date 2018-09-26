# Scala-Machine-Learning

### Architecture

```
Project +-- Algorithm --+-- Classification
        |               |
        |               +-- Clustering
        |               |
        |               +-- Regression
        |               |
        |               +-- DeepLearning
        |               |
        |               +-- Analysis
        |               |
        |               +-- Reinforcement
        |               |
        |               +-- Optimization
        |
        +-- General ----+-- MatrixFunc
```

### Algorithm

```scala
trait Algorithm {
    val algotype: String
    val algoname: String
    val version: String
    def clear: Boolean
    def label: Boolean
    def config(paras: Map[String, Any]): Algorithm
}

trait Classification extends Algorithm {
    val algotype: String = "Classification"
    def train(data: Array[(Int, Array[Double])]): Boolean
    def predict(data: Array[Array[Double]]): Array[Int]
}

class Test(val version: String) extends Classifier {
    val algoname: String = "Test"
    val version: String = "0.0"
    override def clear: Boolean = true
    override def config(paras: Map[String, Any]): Boolean = true
    override def train(data: Array[(Int, Array[Double])]): Boolean = true
    override def predict(data: Array[Array[Double]]): Array[Int] = data.map(_ => 0)
}
```
