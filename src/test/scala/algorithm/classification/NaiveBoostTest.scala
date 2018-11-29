// Wei Chen - Naive Boost Test
// 2018-09-26

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.general.MatrixFunc._
import com.interplanetarytech.algorithm._

class NaiveBoostSuite extends FunSuite {

    val boost = new NaiveBoost()

    test("NaiveBoost Test : Clear") {
        assert(boost.clear())
    }

    test("NaiveBoost Test : Linear Data") {
        assert(boost.clear())
        assert(boost.config(Map[String, Any]()))
        assert(boost.train(LABELED_LINEAR_DATA))
        val result1 = boost.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result1, LABEL_LINEAR_DATA))

        val classifiers: Any = Array(
            new BayesianDecision,
            new DecisionTree,
            new GaussianProcess,
            new KNN,
            new LinearRegression,
            new LinearSVM,
            new Perceptron,
            new RandomForest
        )
        assert(boost.clear())
        assert(boost.config(Map("classifiers" -> classifiers)))
        assert(boost.train(LABELED_LINEAR_DATA))
        val result2 = boost.predict(UNLABELED_LINEAR_DATA)
        assert(arrayequal(result2, LABEL_LINEAR_DATA))
    }

    test("NaiveBoost Test : Nonlinear Data") {
        assert(boost.clear())
        assert(boost.config(Map[String, Any]()))
        assert(boost.train(LABELED_NONLINEAR_DATA))
        val result1 = boost.predict(UNLABELED_NONLINEAR_DATA)
        assert(arrayequal(result1, LABEL_NONLINEAR_DATA))

        val svm = new LinearSVM()
        svm.config(Map("cost" -> Map(1 -> 1.0, 2 -> 1.0)): Map[String, Any])
        val classifiers: Any = Array(
            new BayesianDecision,
            new DecisionTree,
            new GaussianProcess,
            new KNN,
            new LinearRegression,
            svm,
            new Perceptron,
            new RandomForest
        )
        assert(boost.clear())
        assert(boost.config(Map("classifiers" -> classifiers)))
        assert(boost.train(LABELED_NONLINEAR_DATA))
        val result2 = boost.predict(UNLABELED_NONLINEAR_DATA)
        assert(arrayequal(result2, LABEL_NONLINEAR_DATA))
    }
}
