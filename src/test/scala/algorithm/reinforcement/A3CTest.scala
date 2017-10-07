// Wei Chen - Asynchronous Advantage Actor Critic
// 2017-10-01

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.algorithm.A3C

class A3CSuite extends FunSuite {

    val scale = 1
    val limit = 10000
    val epoch = 100

    test("A3C Test : Result 1") { // Not Finished Yet
        def simulator(paras: Array[Double], act: Int): (Array[Double], Double, Boolean) = {
            val links = Map(0 -> Array(1, 2),
                1 -> Array(3, 4))
            val scores = Map(2 -> 10.0, 3 -> 0.0, 4 -> 100.0)
            val atloc = paras.zipWithIndex.maxBy(_._1)._2
            val moves = links.getOrElse(atloc, Array[Int]())
            if (moves.size == 0) {
                null
            } else {
                val endloc = moves(act)
                val result = Array(0.0, 0.0, 0.0, 0.0, 0.0)
                result(endloc) = 1.0
                val nextmoves = links.getOrElse(endloc, Array[Int]())
                (result, scores.getOrElse(endloc, 0.0), nextmoves.size == 0)
            }
        }

        val ql = new A3C(Array(5, 4), Array(5, 4), Array(1.0, 0.0, 0.0, 0.0, 0.0), 2, simulator, 10)
        ql.train(limit, scale, epoch)
        val result = ql.result(epoch)

        assert(result.last.paras.zipWithIndex.maxBy(_._1)._2 == 4)
    }

    test("A3C Test : Result 2") { // Not Finished Yet
        def simulator(paras: Array[Double], act: Int): (Array[Double], Double, Boolean) = {
            val links = Map(0 -> Array(1, 2),
                1 -> Array(3, 4))
            val scores = Map(2 -> 10.0, 3 -> 0.0, 4 -> 12.0)
            val atloc = paras.zipWithIndex.maxBy(_._1)._2
            val moves = links.getOrElse(atloc, Array[Int]())
            if (moves.size == 0) {
                null
            } else {
                val endloc = moves(act)
                val result = Array(0.0, 0.0, 0.0, 0.0, 0.0)
                result(endloc) = 1.0
                val nextmoves = links.getOrElse(endloc, Array[Int]())
                (result, scores.getOrElse(endloc, 0.0), nextmoves.size == 0)
            }
        }

        val ql = new A3C(Array(5, 4), Array(5, 4), Array(1.0, 0.0, 0.0, 0.0, 0.0), 2, simulator, 10)
        ql.train(limit, scale, epoch)
        val result = ql.result(epoch)

        assert(result.last.paras.zipWithIndex.maxBy(_._1)._2 == 4)
    }
}