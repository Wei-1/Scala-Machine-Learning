// Wei Chen - Extreme Learning Test
// 2016-11-06

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.ExtremeLearning
import org.scalatest.funsuite.AnyFunSuite

class ExtremeLearningSuite extends AnyFunSuite {

    val neuronNumber = 10
    val featureNumber = UNLABELED_LARGE_HIGH_DIM_DATA.head.size
    val outputNumber = TARGET_LARGE_HIGH_DIM_DATA.head.size

    val el = new ExtremeLearning(neuronNumber, featureNumber, outputNumber)
    test("ExtremeLearning Test : Initialization") {
        assert(el.wIn.size == featureNumber)
        assert(el.wIn.head.size == neuronNumber)
        assert(el.wOut.size == neuronNumber)
        assert(el.wOut.head.size == outputNumber)
    }

    test("ExtremeLearning Test : Train") {
        el.train(UNLABELED_LARGE_HIGH_DIM_DATA, TARGET_LARGE_HIGH_DIM_DATA)
        val result = el.predict(UNLABELED_LARGE_HIGH_DIM_DATA)
        assert(matrixsimilar(result, TARGET_LARGE_HIGH_DIM_DATA, 0.5))
    }

    test("ExtremeLearning Test : Predict - OVERFITS LIKE CRAZY W/O ENOUGH DATA") {
        val result = el.predict(UNLABELED_SMALL_HIGH_DIM_DATA)
        assert(matrixsimilar(result, TARGET_SMALL_HIGH_DIM_DATA, 1e3))
    }
}
