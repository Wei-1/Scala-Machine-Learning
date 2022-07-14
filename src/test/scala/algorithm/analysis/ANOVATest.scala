// Wei Chen - ANOVA
// 2017-08-30

import com.scalaml.algorithm.ANOVA
import org.scalatest.funsuite.AnyFunSuite

class ANOVASuite extends AnyFunSuite {

    val data = Array(Array(1.1, 1.2), Array(2.1, 2.2))
    val anova = new ANOVA(data)

    test("ANOVA Test : SSG") {
        assert((anova.ssg - 1.0).abs < 0.0000001)
    }

    test("ANOVA Test : SSE") {
        assert((anova.sse - 0.01).abs < 0.0000001)
    }

    test("ANOVA Test : DFG") {
        assert((anova.dfg - 1.0).abs < 0.0000001)
    }

    test("ANOVA Test : DFE") {
        assert((anova.dfe - 2.0).abs < 0.0000001)
    }

    test("ANOVA Test : MSG") {
        assert((anova.msg - 1.0).abs < 0.0000001)
    }

    test("ANOVA Test : MSE") {
        assert((anova.mse - 0.005).abs < 0.0000001)
    }

    test("ANOVA Test : F Value") {
        assert((anova.f - 200.0).abs < 0.0000001)
    }

}
