// Wei Chen - Gene Algorithm Test
// 2017-07-22

import com.scalaml.general.MatrixFunc._
import com.scalaml.algorithm.GeneAlgorithm
import org.scalatest.funsuite.AnyFunSuite

class GeneAlgorithmSuite extends AnyFunSuite {

    val ga = new GeneAlgorithm()

    val initseeds = Array(Array(0.0), Array(1.0))


    def evaluation(arr: Array[Double]): Double = - (arr.head - 0.7).abs

    def breeding(pa: Array[Double], pb: Array[Double]): Array[Array[Double]] =
        Array(pa.zip(pb).map { case (a, b) =>
            if (scala.util.Random.nextDouble > 0.1) (a + b) / 2
            else scala.util.Random.nextDouble
        })

    val generationsize: Int = 100
    val elitesize: Int = 3

    test("GeneAlgorithm Test : Initial") {
        assert(ga.seeds == null)
        ga.setSeeds(initseeds)
        assert(matrixequal(ga.seeds, initseeds))
    }

    test("GeneAlgorithm Test : Evolve") {
        for (i <- 0 until 10)
            ga.evolve(evaluation, breeding, generationsize, elitesize)
        assert(ga.seeds.size == generationsize)

        val best = ga.evolve(evaluation, breeding, generationsize, elitesize)
        assert((best.head - 0.7).abs < 0.05)
    }

}
