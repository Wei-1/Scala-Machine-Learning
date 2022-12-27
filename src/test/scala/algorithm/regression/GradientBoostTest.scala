// Wei Chen - Gradient Boost Test
// 2022-12-27

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm._
import org.scalatest.funsuite.AnyFunSuite

class GradientBoostSuite extends AnyFunSuite {

    val gb = new GradientBoost()
    
    test("GradientBoost Test : Clear") {
        assert(gb.clear())
    }

    test("GradientBoost Test : Linear Data") {
        assert(gb.clear())
        assert(gb.config(Map[String, Any]()))
        assert(gb.train(LABELED_LINEAR_DATA.map(yx => yx._1.toDouble -> yx._2)))
        val result = gb.predict(UNLABELED_LINEAR_DATA)
        val nResult = result.map(v => if (v > 0) 1.0 else -1.0)
        assert(arraysimilar(nResult, LABEL_LINEAR_DATA.map(_.toDouble), 0.9))
    }

    test("GradientBoost Test : Nonlinear Data, 1 Linear Model - WRONG") {
        assert(gb.clear())
        assert(gb.config(Map[String, Any]()))
        assert(gb.train(LABELED_NONLINEAR_DATA.map(yx => yx._1.toDouble -> yx._2)))
        val result = gb.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arraysimilar(result, LABEL_NONLINEAR_DATA.map(_.toDouble), 0.45))
    }

    // More linear regressors will not solve nonlinear problems
    test("GradientBoost Test : Nonlinear Data, 5 Linear Models - WRONG") {
        val regressors: Any = Array(
            new StochasticGradientDecent,
            new StochasticGradientDecent,
            new StochasticGradientDecent,
            new StochasticGradientDecent,
            new StochasticGradientDecent
        )
        assert(gb.clear())
        assert(gb.config(Map("regressors" -> regressors)))
        assert(gb.train(LABELED_NONLINEAR_DATA.map(yx => yx._1.toDouble -> yx._2)))
        val result = gb.predict(UNLABELED_NONLINEAR_DATA)
        assert(!arraysimilar(result, LABEL_NONLINEAR_DATA.map(_.toDouble), 0.45))
    }

    test("GradientBoost Test : Invalid Config & Data") {
        assert(gb.clear())
        assert(!gb.config(Map("regressors" -> "test")))
        assert(!gb.train(Array((1, Array(1, 2)), (1, Array()))))
    }
}
