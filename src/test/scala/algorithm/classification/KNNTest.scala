// Wei Chen - K Nearest Neighborhood Test
// 2016-06-04

import org.scalatest.FunSuite
import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.KNN

class KNNSuite extends FunSuite {

    val knn = new KNN()
    test("KNN Test : Clear") {
        assert(knn.clear())
    }

    test("KNN Test : Linear Data") {
        assert(knn.clear())
        assert(knn.config(Map("k" -> 3.0)))
        assert(knn.train(LABELED_LINEAR_DATA))
        val result = knn.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }
    
    test("KNN Test : Nonlinear Train") {
        assert(knn.clear())
        assert(knn.config(Map("k" -> 1.0)))
        assert(knn.train(LABELED_NONLINEAR_DATA))
        val result = knn.predict(UNLABELED_NONLINEAR_DATA)
        assert(arrayequal(result, LABEL_NONLINEAR_DATA))
    }
}
