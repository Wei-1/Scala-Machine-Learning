// Wei Chen - K Nearest Neighborhood Test
// 2016-06-04

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.KNN

class KNNSuite extends FunSuite {
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

    test("KNN Test : Initialization") {
        val test = new KNN()
        assert(test.referencepoints.isEmpty)
    }

    val knn = new KNN()
    test("KNN Test : Train") {
        knn.train(traindata)
        assert(arrayequal(knn.referencepoints(0)._2, Array(2,5)))
    }
    
    test("KNN Test : Predict") {
        val result = knn.predict(predictdata, 3)
        assert(arrayequal(result.map(_.toDouble), Array(-1,-1,1,1)))
    }
    
    test("KNN Test : Clear") {
        knn.clear()
        assert(knn.referencepoints.isEmpty)
    }
}