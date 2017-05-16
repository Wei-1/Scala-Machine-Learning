// Wei Chen - Deep Belief Network Test
// 2016-11-23

import org.scalatest.FunSuite
import ght.mi.TestData._
import ght.mi.algorithm.MatrixFunc._
import ght.mi.algorithm.LstmParam
import ght.mi.algorithm.LstmNetwork

class LSTMSuite extends FunSuite {

    val learning_rate: Double = 0.1

    val loss_func = (a1: Array[Double], a2: Array[Double]) => arrayminussquare(a1, a2).sum
    val diff_func = (a1: Array[Double], a2: Array[Double]) =>
        arrayminus(a1, a2).map(_ * 2) ++ new Array[Double](a1.size - a2.size)

    test("LSTM Test : Random Assignment") {

        val mem_cell_ct = 100
        val x_dim = 50
        val lstm_param = new LstmParam(mem_cell_ct, x_dim)
        val lstm_net = new LstmNetwork(lstm_param)
        val y_length = 4
        val y_list = Array(Array(-0.5), Array(0.2), Array(0.1), Array(-0.5))
        val x_list = matrixrandom(4, x_dim, -1, 1)
        val limit = 100
        var loss = y_length.toDouble

        var cur_iter = 0
        while (cur_iter < limit) {
            cur_iter += 1
            lstm_net.set_x_list(x_list)
            val newloss = lstm_net.set_y_list(y_list, loss_func, diff_func)
            if (newloss > loss * 2 && newloss > y_length) {
                cur_iter = 0
                lstm_param.clear_wb()
                Console.err.println("[log] RESET PARAM: " + newloss)
            } else {
                lstm_param.apply_diff(learning_rate)
            }
            lstm_net.clear()
            loss = newloss
        }

        assert(loss < 0.001)
    }

    test("LSTM Test : 2x2 + Continous Exclusive OR") {

        val mem_cell_ct = 4
        val x_dim = 2
        val lstm_param = new LstmParam(mem_cell_ct, x_dim)
        val lstm_net = new LstmNetwork(lstm_param)

        val y_length = 40
        val x_list = matrixrandom(y_length, x_dim, 0, 2).map(_.map(Math.floor(_)))
        var last = (false, false)
        var y_list = Array[Array[Double]]()

        for (i <- 0 until y_length) {
            val yt = x_list(0) != x_list(1)
            val y0 = last._1 != yt
            val y1 = last._2 != yt
            val y0v = if (y0) 1.0 else 0.0
            val y1v = if (y1) 1.0 else 0.0
            y_list :+= Array(y0v, y1v)
            last = (yt, y1)
        }

        val limit = 1000
        var loss = y_length.toDouble

        var cur_iter = 0
        while (cur_iter < limit) {
            cur_iter += 1
            lstm_net.set_x_list(x_list)
            val newloss = lstm_net.set_y_list(y_list, loss_func, diff_func)
            if (newloss > loss * 2 && newloss > y_length) {
                cur_iter = 0
                lstm_param.clear_wb()
                Console.err.println("[log] RESET PARAM: " + newloss)
            } else {
                lstm_param.apply_diff(learning_rate)
            }
            lstm_net.clear()
            loss = newloss
        }
        assert(loss < 0.05)
    }
}