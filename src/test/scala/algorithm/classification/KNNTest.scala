// Wei Chen - K Nearest Neighborhood Test
// 2016-06-04

import org.scalatest.FunSuite
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.KNN

class KNNSuite extends FunSuite {
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

    test("KNN Test : Initialization"){
        val test = new KNN()
        assert(test.referencepoints.isEmpty)
    }

    val knn = new KNN()
    test("KNN Test : Train"){
        knn.train(traindata)
        assert(arrayequal(knn.referencepoints(0)._2, Array(2,5)))
    }
    
    test("KNN Test : Predict"){
        val result = knn.predict(predictdata, 3)
        assert(arrayequal(result.map(_.toDouble), Array(-1,-1,1,1)))
    }
    
    test("KNN Test : Clear"){
        knn.clear()
        assert(knn.referencepoints.isEmpty)
    }
}