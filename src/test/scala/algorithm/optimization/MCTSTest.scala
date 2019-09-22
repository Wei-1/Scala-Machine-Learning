// Wei Chen - Monte Carlo Tree Search Test
// 2017-08-08

import org.scalatest.FunSuite
import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.MCTS

class MCTSSuite extends FunSuite {
    
    {
        val mcts = new MCTS

        def simulation(state: Array[Double]): Double = {
            val statei = state.zipWithIndex
            val myi = statei.maxBy(_._1)._2
            val eni = statei.minBy(_._1)._2
            if (myi == eni) -1.0
            else 1.0
        }

        def actions(state: Array[Double]): Array[Array[Double]] = {
            val statei = state.zipWithIndex
            val myi = statei.maxBy(_._1)._2
            val eni = statei.minBy(_._1)._2
            val newstate = Array(0.0, 0.0, 0.0, 0.0)
            if (myi == eni) {
                Array(newstate)
            } else {
                val mymoves = Array((myi + 3) % 4, myi, (myi + 1) % 4)
                val enmoves = Array((eni + 3) % 4, eni, (eni + 1) % 4)
                mymoves.map { i =>
                    if (enmoves.contains(i)) {
                        newstate.clone
                    } else {
                        val fei = enmoves.minBy(ei => (i - ei).abs % 3)
                        val ns = newstate.clone
                        ns(i) = 1.0
                        ns(fei) = -1.0
                        ns
                    }
                }
            }
        }

        test("MCTS Test : Search Test 1") {
            val state = Array(-1.0, 1.0, 0.0, 0.0)
            val iter: Int = 4

            val node = mcts.search(simulation, actions, state, iter)

            assert(arrayequal(node.init, Array(0.0, -1.0, 1.0, 0.0)) || arrayequal(node.init, Array(0.0, 0.0, 1.0, -1.0)))
            assert(node.score == 1.0)
        }

        test("MCTS Test : Search Test 2") {
            val state = Array(-1.0, 0.0, 1.0, 0.0)
            val iter: Int = 2

            val node = mcts.search(simulation, actions, state, iter)

            assert(arrayequal(node.init, Array(0.0, -1.0, 1.0, 0.0)) || arrayequal(node.init, Array(0.0, 0.0, 1.0, -1.0)))
            assert(node.score == 1.0)
        }
    }
    
    {
        val mcts = new MCTS

        def simulation(state: Array[Double]): Double = {
            val statei = state.zipWithIndex
            val i = statei.maxBy(_._1)._2
            i match {
                case 0 => 1
                case 1 => 2
                case 2 => 3
                case 3 => 0
            }
        }

        def actions(state: Array[Double]): Array[Array[Double]] = {
            val statei = state.zipWithIndex
            val i = statei.maxBy(_._1)._2
            i match {
                case 0 => Array(Array(0, 1, 0, 0), Array(0, 0, 1, 0))
                case 1 => Array(Array(0, 1, 0, 0))
                case 2 => Array(Array(0, 0, 0, 1))
                case 3 => Array(Array(0, 0, 0, 1))
            }
        }

        test("MCTS Test : Search Test 3") {
            val state = Array(1.0, 0.0, 0.0, 0.0)
            val iter: Int = 4

            val node = mcts.search(simulation, actions, state, iter)

            assert(arrayequal(node.init, Array(0.0, 1.0, 0.0, 0.0)))
            assert(node.score == 2.0)
        }
    }

}
