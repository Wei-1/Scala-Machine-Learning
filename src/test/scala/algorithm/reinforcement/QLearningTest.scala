// Wei Chen - Q-Learning Test
// 2017-07-28

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.algorithm.QLearning

class QLearningSuite extends FunSuite {

    val learning_rate = 0.1
    val scale = 1
    val limit = 10000
    val epoch = 100

    val statenumber = 5
    val scores = Map(2 -> 10.0, 3 -> 0.0, 4 -> 100.0)
    val links = Map(0 -> Array(1, 2),
        1 -> Array(3, 4))

    val ql = new QLearning(statenumber)
    ql.addScores(scores)
    ql.addLinks(links)

    test("QLearning Test : Iterate") {
        ql.iterate(limit, learning_rate, scale, epoch)
        assert(true)
    }

    test("QLearning Test : Result") {
        val result = ql.result(epoch)
        assert(result.size == 3)
        assert(result.last.id == 4)
    }
}