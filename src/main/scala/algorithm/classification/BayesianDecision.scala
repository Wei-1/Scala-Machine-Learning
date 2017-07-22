// Wei Chen - Bayesian Decision
// 2015-12-20

package ght.mi.algorithm
import ght.mi.general.MatrixFunc._

class BayesianDecision() {

    var groupcnt = Map[Int, Int]()
    var groupavg = Map[Int, Array[Double]]()
    var groupcov = Map[Int, Array[Array[Double]]]()

    def clear() = {
        groupcnt = Map[Int, Int]()
        groupavg = Map[Int, Array[Double]]()
        groupcov = Map[Int, Array[Array[Double]]]()
    }

    private def arrstats(
        i: Int,
        data: Array[Array[Double]]
    ): Int = {
        val n = data.size
        val m = matrixaccumulate(data).map(_/n)
        val s = covariance(data)
        groupcnt += (i -> n)
        groupavg += (i -> m)
        groupcov += (i -> s)
        return n
    }

    def train(
        data: Array[(Int, Array[Double])]
    ): Array[Int] = {
        return data.groupBy(_._1)
            .map { d =>
                arrstats(d._1, d._2.map(_._2))
            }.toArray
    }

    def predict(
        data: Array[Array[Double]]
    ): Array[Int] = {
        val n = groupavg.size
        if (n == 0) {
            return Array[Int]()
        }else{
            return data.map { d =>
                groupcnt.map { cnt =>
                    val i = cnt._1
                    (i, cnt._2 * gaussianprobability(d, groupavg(i), groupcov(i)))
                }.toArray
                    .maxBy(_._2)._1
            }
        }
    }
}
