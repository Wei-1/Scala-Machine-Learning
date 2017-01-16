// Wei Chen - 2017-01-16
package ght.mi.algorithm

import ght.mi.algorithm.MatrixFunc._

class LstmParam(val mem_cell_ct: Int, val x_dim: Int) {
    val concat_len = x_dim + mem_cell_ct
    // Weightings + Biases
    var wg = matrixrandom(mem_cell_ct, concat_len, -0.1, 0.1)
    var wi = matrixrandom(mem_cell_ct, concat_len, -0.1, 0.1)
    var wf = matrixrandom(mem_cell_ct, concat_len, -0.1, 0.1)
    var wo = matrixrandom(mem_cell_ct, concat_len, -0.1, 0.1)
    var bg = arrayrandom(mem_cell_ct, -0.1, 0.1)
    var bi = arrayrandom(mem_cell_ct, -0.1, 0.1)
    var bf = arrayrandom(mem_cell_ct, -0.1, 0.1)
    var bo = arrayrandom(mem_cell_ct, -0.1, 0.1)
    // Differences
    var wg_diff = Array.ofDim[Double](mem_cell_ct, concat_len)
    var wi_diff = Array.ofDim[Double](mem_cell_ct, concat_len)
    var wf_diff = Array.ofDim[Double](mem_cell_ct, concat_len)
    var wo_diff = Array.ofDim[Double](mem_cell_ct, concat_len)
    var bg_diff = new Array[Double](mem_cell_ct)
    var bi_diff = new Array[Double](mem_cell_ct)
    var bf_diff = new Array[Double](mem_cell_ct)
    var bo_diff = new Array[Double](mem_cell_ct)

    def clear_diff() {
        wg_diff = Array.ofDim[Double](mem_cell_ct, concat_len)
        wi_diff = Array.ofDim[Double](mem_cell_ct, concat_len)
        wf_diff = Array.ofDim[Double](mem_cell_ct, concat_len)
        wo_diff = Array.ofDim[Double](mem_cell_ct, concat_len)
        bg_diff = new Array[Double](mem_cell_ct)
        bi_diff = new Array[Double](mem_cell_ct)
        bf_diff = new Array[Double](mem_cell_ct)
        bo_diff = new Array[Double](mem_cell_ct)
    }

    def apply_diff(lr: Double) {
        for (i <- 0 until mem_cell_ct) {
            for (j <- 0 until concat_len) {
                wg(i)(j) -= lr * wg_diff(i)(j)
                wi(i)(j) -= lr * wi_diff(i)(j)
                wf(i)(j) -= lr * wf_diff(i)(j)
                wo(i)(j) -= lr * wo_diff(i)(j)
            }
            bg(i) -= lr * bg_diff(i)
            bi(i) -= lr * bi_diff(i)
            bf(i) -= lr * bf_diff(i)
            bo(i) -= lr * bo_diff(i)
        }
        clear_diff()
    }
}

class LstmState(val mem_cell_ct: Int, val x_dim: Int) {
    var g = new Array[Double](mem_cell_ct)
    var i = new Array[Double](mem_cell_ct)
    var f = new Array[Double](mem_cell_ct)
    var o = new Array[Double](mem_cell_ct)
    var s = new Array[Double](mem_cell_ct)
    var h = new Array[Double](mem_cell_ct)
    var bottom_diff_h = new Array[Double](mem_cell_ct)
    var bottom_diff_s = new Array[Double](mem_cell_ct)
    var bottom_diff_x = new Array[Double](x_dim)
}

class LstmNode(val param: LstmParam, val state: LstmState) {
    val mem_cell_ct = param.mem_cell_ct
    val x_dim = param.x_dim
    val concat_len = param.concat_len
    var s_prev = new Array[Double](mem_cell_ct)
    var h_prev = new Array[Double](mem_cell_ct)
    var xc = new Array[Double](concat_len)

    def tanh_arr(x: Array[Double]): Array[Double] =
        return x.map(v => Math.tanh(v))

    def sigmoid_arr(x: Array[Double]): Array[Double] =
        return x.map(v => 1.0 / (1 + Math.exp(-v)))

    def dot_wxb(w: Array[Array[Double]], x: Array[Double], b: Array[Double]): Array[Double] =
        return w.zip(b).map { case (arr, bi) =>
            arr.zip(x).map(multifunc).sum + bi
        }

    def dot_wd(w: Array[Array[Double]], d: Array[Double]): Array[Double] =
        return matrixaccumulate(w.zip(d).map { case (arr, di) => arr.map(_ * di) })

    def diff_arr(arr: Array[Double], diff: Array[Double]): Array[Double] =
        return arr.zip(diff).map { case (a, d) => (1 - a) * a * d }

    def outer_arr(a1: Array[Double], a2: Array[Double]): Array[Array[Double]] =
        return a1.map(a => a2.map(_ * a))

    def bottom_data_is(x: Array[Double], s_prev_in: Array[Double], h_prev_in: Array[Double]) {
        s_prev = s_prev_in
        h_prev = h_prev_in
        xc = x ++ h_prev_in
        state.g = tanh_arr(dot_wxb(param.wg, xc, param.bg))
        state.i = sigmoid_arr(dot_wxb(param.wi, xc, param.bi))
        state.f = sigmoid_arr(dot_wxb(param.wf, xc, param.bf))
        state.o = sigmoid_arr(dot_wxb(param.wo, xc, param.bo))
        state.s = arraysum(arraymultiply(state.g, state.i), arraymultiply(s_prev_in, state.f))
        state.h = arraymultiply(state.s, state.o)
    }

    def top_diff_is(top_diff_h: Array[Double], top_diff_s: Array[Double]) {
        val d_s = arraysum(arraymultiply(state.o, top_diff_h), top_diff_s)
        val d_o = arraymultiply(state.s, top_diff_h)
        val d_i = arraymultiply(state.g, d_s)
        val d_g = arraymultiply(state.i, d_s)
        val d_f = arraymultiply(s_prev, d_s)

        val d_i_input = diff_arr(state.i, d_i)
        val d_f_input = diff_arr(state.f, d_f)
        val d_o_input = diff_arr(state.o, d_o)
        val d_g_input = state.g.zip(d_g).map { case (a, d) => (1 - Math.pow(a, 2)) * d }

        param.wi_diff = matrixsum(param.wi_diff, outer_arr(d_i_input, xc))
        param.wf_diff = matrixsum(param.wf_diff, outer_arr(d_f_input, xc))
        param.wo_diff = matrixsum(param.wo_diff, outer_arr(d_o_input, xc))
        param.wg_diff = matrixsum(param.wg_diff, outer_arr(d_g_input, xc))
        param.bi_diff = arraysum(param.bi_diff, d_i_input)
        param.bf_diff = arraysum(param.bf_diff, d_f_input)
        param.bo_diff = arraysum(param.bo_diff, d_o_input)
        param.bg_diff = arraysum(param.bg_diff, d_g_input)

        val dxc = matrixaccumulate(Array(
            dot_wd(param.wi, d_i_input),
            dot_wd(param.wf, d_f_input),
            dot_wd(param.wo, d_o_input),
            dot_wd(param.wg, d_g_input)))

        val (b_d_x, b_d_h) = dxc.splitAt(x_dim)
        state.bottom_diff_x = b_d_x
        state.bottom_diff_h = b_d_h
        state.bottom_diff_s = arraymultiply(d_s, state.f)
    }
}

class LstmNetwork(val param: LstmParam) {
    var node_list = Array[LstmNode]()
    var x_list = Array[Array[Double]]()

    def y_list_is(y_list: Array[Array[Double]], loss_func: (Array[Double], Array[Double]) => Double, diff_func: (Array[Double], Array[Double]) => Array[Double]): Double = {
        if (y_list.size == x_list.size) {
            var idx = x_list.size - 1
            var h = node_list(idx).state.h
            var y = y_list(idx)

            var loss = loss_func(h, y)
            var diff_h = diff_func(h, y)
            var diff_s = new Array[Double](param.mem_cell_ct)
            node_list(idx).top_diff_is(diff_h, diff_s)
            idx -= 1

            while (idx >= 0) {
                h = node_list(idx).state.h
                y = y_list(idx)
                val p1_state = node_list(idx + 1).state
                loss += loss_func(h, y)
                diff_h = arraysum(diff_func(h, y), p1_state.bottom_diff_h)
                diff_s = p1_state.bottom_diff_s
                node_list(idx).top_diff_is(diff_h, diff_s)
                idx -= 1
            }

            return loss
        } else {
            System.err.println("Y_LIST_SIZE != X_LIST_SIZE")
            System.exit(1)
            return 0
        }
    }

    def x_list_clear() {
        x_list = Array[Array[Double]]()
    }

    def x_list_add(x: Array[Double]) {
        x_list :+= x
        if (x_list.size > node_list.size) {
            val state = new LstmState(param.mem_cell_ct, param.x_dim)
            node_list :+= (new LstmNode(param, state))
        }

        val idx = x_list.size - 1
        if (idx == 0) {
            val s_prev = new Array[Double](param.mem_cell_ct)
            val h_prev = new Array[Double](param.mem_cell_ct)
            node_list(0).bottom_data_is(x, s_prev, h_prev)
        } else {
            val n1_state = node_list(idx - 1).state
            val s_prev = n1_state.s
            val h_prev = n1_state.h
            node_list(idx).bottom_data_is(x, s_prev, h_prev)
        }
    }
}
