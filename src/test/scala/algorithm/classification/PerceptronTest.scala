// Wei Chen - Perceptron Test
// 2016-06-04

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.Perceptron

class PerceptronSuite extends FunSuite {
    val traindata = Array(
        Array(-1,2,5),
        Array(-1,3,4),
        Array(-1,4,5),
        Array(1,5,4),
        Array(1,6,5),
        Array(1,7,4)
    ).map(d => (d(0), d.drop(1).map(_.toDouble)))
    val predictdata: Array[Array[Double]] = Array(
        Array(0,4),
        Array(1,4),
        Array(8,5),
        Array(9,5)
    )
    val lambda = 1.5
    val limit = 1000

    test("Perceptron Test : Initialization"){
        val test = new Perceptron()
        assert(test.projector.isEmpty)
    }

    val perceptron = new Perceptron()
    test("Perceptron Test : Train"){
        perceptron.train(traindata, lambda, limit)
        assert(perceptron.projector(0)._1 == -1)
        assert(perceptron.projector(0)._2 == 1)
    }

    test("Perceptron Test : Predict"){
        val result = perceptron.predict(predictdata)
        assert(arrayequal(result.map(_.toDouble), Array(-1,-1,1,1)))
    }
    
    test("Perceptron Test : Clear"){
        perceptron.clear()
        assert(perceptron.projector.isEmpty)
    }
}