// Wei Chen - K Nearest Neighborhood Test
// 2016-06-04

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.KNN

class KNNSuite extends FunSuite {

    val knn = new KNN()
    test("KNN Test : Initialization") {
        assert(knn.referencepoints.isEmpty)
    }

    test("KNN Test : Linear Train") {
        knn.train(LABELED_LINEAR_DATA)
        assert(!knn.referencepoints.isEmpty)
    }
    
    test("KNN Test : Linear Predict") {
        val result = knn.predict(UNLABELED_LINEAR_DATA, 3)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("KNN Test : Nonlinear Train") {
        knn.train(LABELED_NONLINEAR_DATA)
        assert(!knn.referencepoints.isEmpty)
    }
    
    test("KNN Test : Nonlinear Predict") {
        val result = knn.predict(UNLABELED_NONLINEAR_DATA, 1)
        assert(arrayequal(result, LABEL_NONLINEAR_DATA))
    }
    
    test("KNN Test : Clear") {
        knn.clear()
        assert(knn.referencepoints.isEmpty)
    }
}