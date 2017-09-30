// Wei Chen - Quadratic Discriminant Analysis
// 2017-09-01

import org.scalatest.FunSuite
import ght.mi.algorithm.QDA

class QDASuite extends FunSuite {

    val d1 = Array(Array(1.0, 2.1), Array(2.0, 3.2), Array(0.0, 0.0))
    val d2 = Array(Array(2.0, 1.1), Array(3.0, 2.2), Array(4.0, 4.0))

    val qda = new QDA(d1, d2)
    val result = qda.predict(Array(Array(1.0, 1.0), Array(5.0, 5.0)))

    test("LDA Test : Prediction") {
        assert(result(0) == 1)
        assert(result(1) == 2)
    }

}
