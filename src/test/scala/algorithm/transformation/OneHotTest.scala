// Wei Chen - OneHot
// 2018-11-08

import org.scalatest.FunSuite
import com.scalaml.algorithm.OneHot

class OneHotSuite extends FunSuite {

    val table = Array(("key", "category"), ("value", "number"))
    val data = Array(Array("A", "1.1"), Array("B", "1.2"))
    val result = Array(Array(0.0, 1.0, 2.1), Array(1.0, 0.0, 2.2))
    val oh = new OneHot(table)

    test("OneHot Test : Encode") {
        val encodeData = oh.encode(data)
        assert(encodeData.head(0) == 1.0)
        assert(encodeData.head(1) == 0.0)
        assert(encodeData.head(2) == 1.1)
        assert(encodeData.last(0) == 0.0)
        assert(encodeData.last(1) == 1.0)
        assert(encodeData.last(2) == 1.2)
    }

    test("OneHot Test : Decode") {
        val decodeData = oh.decode(result)
        Array(Array("B", "2.1"), Array("A", "2.2"))
        assert(decodeData.head(0) == "B")
        assert(decodeData.head(1) == "2.1")
        assert(decodeData.last(0) == "A")
        assert(decodeData.last(1) == "2.2")
    }
}
