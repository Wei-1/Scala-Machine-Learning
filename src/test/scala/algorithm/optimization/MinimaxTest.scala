// Wei Chen - Minimax Test
// 2017-07-22

import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.Minimax
import org.scalatest.funsuite.AnyFunSuite

class MinimaxSuite extends AnyFunSuite {

    val minimax = new Minimax()
        
    def evaluation(mystate: Array[Double], enstate: Array[Double]): Double = {
        val myi = mystate.zipWithIndex.maxBy(_._1)._2
        val eni = enstate.zipWithIndex.maxBy(_._1)._2
        if (myi == eni) -1.0
        else 1.0
    }

    def moves(state: Array[Double]): Array[Array[Double]] = {
        val i = state.zipWithIndex.maxBy(_._1)._2
        val newstate = Array(0.0, 0.0, 0.0, 0.0)
        Array({
            val ns = newstate.clone
            ns((i + 3) % 4) = 1.0
            ns
        },{
            val ns = newstate.clone
            ns(i) = 1.0
            ns
        },{
            val ns = newstate.clone
            ns((i + 1) % 4) = 1.0
            ns
        })
    }

    test("Minimax Test : Search Test 1") {
        val mystate = Array(0.0, 1.0, 0.0, 0.0)
        val enstate = Array(1.0, 0.0, 0.0, 0.0)

        val iter: Int = 2
        val whos: Boolean = true

        val (result, score) = minimax.search(null, mystate, enstate, evaluation, moves, iter, whos)

        assert(arrayequal(result, Array(0.0, 0.0, 1.0, 0.0)))
        assert(score == 1.0)
    }

    test("Minimax Test : Search Test 2") {
        val mystate = Array(1.0, 0.0, 0.0, 0.0)
        val enstate = Array(0.0, 0.0, 1.0, 0.0)

        val iter: Int = 4
        val whos: Boolean = true

        val (result, score) = minimax.search(null, mystate, enstate, evaluation, moves, iter, whos)

        assert(arrayequal(result, Array(1.0, 0.0, 0.0, 0.0)))
        assert(score == 1.0)
    }

}
