// Wei Chen - Q-Learning Test
// 2017-08-16

import com.scalaml.TestData._
import com.scalaml.algorithm.QLearning
import org.scalatest.funsuite.AnyFunSuite

class QLearningSuite extends AnyFunSuite {

    val learning_rate = 0.1
    val scale = 1
    val limit = 10000
    val epoch = 100

    val statenumber = 5
    val links = Map(0 -> Array(1, 2),
        1 -> Array(3, 4))

    { // Case 1
        val scores = Map(2 -> 10.0, 3 -> 0.0, 4 -> 100.0)
        val ql = new QLearning(statenumber)
        ql.addRewards(scores)
        ql.addLinks(links)

        test("QLearning Test : Iteration") {
            ql.iterate(limit, learning_rate, scale, epoch)
            assert(true)
        }

        test("QLearning Test : Result 1") {
            val result = ql.result(epoch)
            assert(result.size == 3)
            assert(result.last.id == 4)
        }
    }

    { // Case 2
        val scores = Map(2 -> 10.0, 3 -> 0.0, 4 -> 12.0)
        val ql = new QLearning(statenumber)
        ql.addRewards(scores)
        ql.addLinks(links)
        ql.iterate(limit, learning_rate, scale, epoch)

        test("QLearning Test : Result 2") {
            val result = ql.result(epoch)
            assert(result.size == 3)
            assert(result.last.id == 4)
        }
    }
}
