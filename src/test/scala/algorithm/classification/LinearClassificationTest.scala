// Wei Chen - Linear Classification Test
// 2016-06-04

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.LinearClassification
import org.scalatest.funsuite.AnyFunSuite

class LinearClassificationSuite extends AnyFunSuite {

    val linearC = new LinearClassification()
    
    test("LinearClassification Test : Clear") {
        assert(linearC.clear())
    }

    test("LinearClassification Test : Linear Data") {
        assert(linearC.clear())
        assert(linearC.config(Map[String, Double]()))
        assert(linearC.train(LABELED_LINEAR_DATA))
        val result = linearC.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result, LABEL_LINEAR_DATA))
    }

    test("LinearClassification Test : Nonlinear Data - WRONG") {
        assert(linearC.clear())
        assert(linearC.config(Map[String, Double]()))
        assert(linearC.train(LABELED_NONLINEAR_DATA))
        val result = linearC.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arrayequal(result, LABEL_NONLINEAR_DATA))
    }

}
