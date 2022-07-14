// Wei Chen - Student T
// 2017-07-25

import com.scalaml.TestData._
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.StudentT
import org.scalatest.funsuite.AnyFunSuite

class StudentTSuite extends AnyFunSuite {

    val st = new StudentT()
    val arr1 = UNLABELED_LARGE_DATA.map(_.head)
    val arr2 = UNLABELED_LARGE_DATA.map(_.last)

    test("StudentT Test : One Sample") {
        assert(st.oneSample(arr1, 4.5) < 1)
        assert(st.oneSample(arr1, 15) > 20)
    }

    test("StudentT Test : Two Sample") {
        assert(st.twoSample(arr1, arr1) < 1)
        assert(st.twoSample(arr1, arr2) > 16)
    }
    
    test("StudentT Test : No Data") {
        assert(st.oneSample(Array(), 0) == 0.0)
        assert(st.twoSample(Array(), Array(1.0)) == 0.0)
        assert(st.twoSample(Array(1.0), Array()) == 0.0)
    }

}
