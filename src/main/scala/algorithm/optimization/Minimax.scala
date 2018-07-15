// Wei Chen - Minimax
// 2017-07-22

package com.interplanetarytech.algorithm

class Minimax {

    def search(
        myfirst: Array[Double] = null,
        mystate: Array[Double] = null,
        enstate: Array[Double] = null,
        evaluation: (Array[Double], Array[Double]) => Double,
        moves: Array[Double] => Array[Array[Double]],
        iter: Int = 2,
        whos: Boolean
    ): (Array[Double], Double) = {
        if (iter == 0) {
            return (myfirst, evaluation(mystate, enstate))
        } else {
            if (whos) {
                return moves(mystate).map(move => search(
                    if (myfirst == null) move
                    else myfirst
                , move, enstate, evaluation, moves, iter - 1, false)).maxBy(_._2)
            } else {
                return moves(enstate).map(move => search(myfirst, mystate, move, evaluation, moves, iter - 1, true)).minBy(_._2)
            }
        }
    }

}
