// Wei Chen - Multiple Linear Regression Test
// 2016-06-04

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.RegressionTree
import org.scalatest.funsuite.AnyFunSuite

class RegressionTreeSuite extends AnyFunSuite {

    val rt = new RegressionTree()
    
    test("RegressionTree Test : Clear") {
        assert(rt.clear())
    }

    test("RegressionTree Test : Linear Data") {
        assert(rt.clear())
        assert(rt.config(Map[String, Double]()))
        assert(rt.train(LABELED_LINEAR_DATA.map(yx => yx._1.toDouble -> yx._2)))
        val result = rt.predict(UNLABELED_LINEAR_DATA)
        assert(arraysimilar(result, LABEL_LINEAR_DATA.map(_.toDouble), 0.9))
        Console.err.println(result.mkString(","), LABEL_LINEAR_DATA.mkString(","))
    }

    test("RegressionTree Test : Nonlinear Data - WRONG") {
        assert(rt.clear())
        assert(rt.config(Map[String, Double]()))
        assert(rt.train(LABELED_NONLINEAR_DATA.map(yx => yx._1.toDouble -> yx._2)))
        val result = rt.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arraysimilar(result, LABEL_NONLINEAR_DATA.map(_.toDouble), 0.45))
    }

    test("RegressionTree Test : Invalid Data") {
        assert(rt.clear())
        assert(!rt.train(Array((1, Array(1, 2)), (1, Array()))))
    }
}
