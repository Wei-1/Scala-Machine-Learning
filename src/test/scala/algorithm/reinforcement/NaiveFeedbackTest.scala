// Wei Chen - Naive Feedback Learning Test
// 2017-07-28

import org.scalatest.FunSuite
import com.interplanetarytech.TestData._
import com.interplanetarytech.algorithm.NaiveFeedback

class NaiveFeedbackSuite extends FunSuite {

    val learning_rate = 0.1
    val scale = 1
    val limit = 10000
    val epoch = 100

    val statenumber = 5
    val links = Map(0 -> Array(1, 2),
        1 -> Array(3, 4))

    { // Case 1
        val scores = Map(2 -> 10.0, 3 -> 0.0, 4 -> 100.0)
        val nf = new NaiveFeedback(statenumber)
        nf.addScores(scores)
        nf.addLinks(links)

        test("NaiveFeedback Test : Iteration") {
            nf.iterate(limit, learning_rate, scale, epoch)
            assert(true)
        }

        test("NaiveFeedback Test : Result 1") {
            val result = nf.result(epoch)
            assert(result.size == 3)
            assert(result.last.id == 4)
        }
    }

    { // Case 2
        val scores = Map(2 -> 10.0, 3 -> 0.0, 4 -> 12.0)
        val nf = new NaiveFeedback(statenumber)
        nf.addScores(scores)
        nf.addLinks(links)
        nf.iterate(limit, learning_rate, scale, epoch)

        test("NaiveFeedback Test : Result 2 - WRONG") {
            val result = nf.result(epoch)
            assert(result.size == 2)
            assert(result.last.id == 2)
        }
    }
}
