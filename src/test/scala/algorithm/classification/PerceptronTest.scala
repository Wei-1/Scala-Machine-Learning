// Wei Chen - Perceptron Test
// 2016-06-04

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.Perceptron

class PerceptronSuite extends FunSuite {
    val traindata: Array[(Int, Array[Double])] = Array(
        (-1, Array(2,5)),
        (-1, Array(3,4)),
        (-1, Array(4,5)),
        (1, Array(5,4)),
        (1, Array(6,5)),
        (1, Array(7,4)))

    val predictdata: Array[Array[Double]] = Array(
        Array(0,4),
        Array(1,4),
        Array(8,5),
        Array(9,5))
    
    val lambda = 1.5
    val limit = 1000

    test("Perceptron Test : Initialization") {
        val test = new Perceptron()
        assert(test.projector.isEmpty)
    }

    val perceptron = new Perceptron()
    test("Perceptron Test : Train") {
        perceptron.train(traindata, lambda, limit)
        assert(perceptron.projector(0)._1 == -1)
        assert(perceptron.projector(0)._2 == 1)
    }

    test("Perceptron Test : Predict") {
        val result = perceptron.predict(predictdata)
        assert(arrayequal(result.map(_.toDouble), Array(-1,-1,1,1)))
    }
    
    test("Perceptron Test : Clear") {
        perceptron.clear()
        assert(perceptron.projector.isEmpty)
    }
}