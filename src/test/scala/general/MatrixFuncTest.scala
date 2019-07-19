// Wei Chen - Matrix Function Test
// 2016-06-03

import org.scalatest.FunSuite
import com.scalaml.general.MatrixFunc._

class MatrixFuncSuite extends FunSuite {
    def rint4(x: Double): Double = Math.rint(x * 10000) / 10000
    def arrrint4(x: Array[Double]): Array[Double] = x.map(rint4(_))
    val data: Array[Array[Double]] = Array(
        Array(1,2,1),
        Array(2,1,6),
        Array(1,2,6),
        Array(0,3,1),
        Array(1,3,1)
    )
    val datasize = data.size
    val featuresize = data.head.size

    test("MatrixFunc Test : Array Similarity") {
        assert(!arraysimilar(data(0), data(1), 2))
        assert(arraysimilar(data(0), Array(2.0,3.0,2.0), 2))
    }

    test("MatrixFunc Test : Array Equality") {
        assert(!arrayequal(data(0), data(1)))
        assert(arrayequal(data(0), Array(1.0,2.0,1.0)))
        assert(arrayequal(Array(1,2), Array(1,2)))
    }

    test("MatrixFunc Test : Array Sum") {
        assert(arrayequal(arraysum(data(0),data(1)), Array(3.0,3.0,7.0)))
    }

    test("MatrixFunc Test : Array Minus") {
        assert(arrayequal(arrayminus(data(0),data(1)), Array(-1.0,1.0,-5.0)))
    }

    test("MatrixFunc Test : Array Minus Square") {
        assert(arrayequal(arrayminussquare(data(0),data(1)), Array(1.0,1.0,25.0)))
    }

    test("MatrixFunc Test : Array Multiplication") {
        assert(arrayequal(arraymultiply(data(0),data(1)), Array(2.0,2.0,6.0)))
    }

    test("MatrixFunc Test : Array Devision") {
        assert(arrayequal(arraydevide(data(0),data(1)), Array(0.5,2.0,1.0/6.0)))
        assert(arrayequal(arraydevide(data(2),data(3)), Array(0.0,2.0/3.0,6.0)))
    }

    test("MatrixFunc Test : Matrix Similarity") {
        val temp1 = Array(Array(0.1, 0.2), Array(0.0, 1.0))
        val temp2 = Array(Array(0.6, 0.7), Array(0.5, 1.5))
        assert(matrixsimilar(temp1, temp2, 1))
    }

    test("MatrixFunc Test : Matrix Equality") {
        val temp = Array(Array(0.1, 0.2), Array(0.0, 1.0))
        assert(matrixequal(temp, temp))
    }

    test("MatrixFunc Test : Matrix Accu") {
        assert(arrayequal(matrixaccumulate(data), Array(5.0,11.0,15.0)))
    }

    test("MatrixFunc Test : Matrix Sum") {
        val temp1 = Array(Array(0.1, 0.2), Array(0.0, 1.0))
        val temp2 = Array(Array(0.2, 0.4), Array(0.0, 2.0))
        assert(matrixsimilar(temp2, matrixsum(temp1, temp1), 0.01))
    }

    test("MatrixFunc Test : Matrix Minus") {
        val temp1 = Array(Array(0.1, 0.2), Array(0.0, 1.0))
        val temp2 = Array(Array(0.0, 0.0), Array(0.0, 0.0))
        assert(matrixsimilar(temp2, matrixminus(temp1, temp1), 0.01))
    }

    test("MatrixFunc Test : Matrix Multiplication") {
        val temp1 = Array(Array(0.1, 0.2), Array(0.0, 1.0))
        val temp2 = Array(Array(0.01, 0.04), Array(0.0, 1.0))
        assert(matrixsimilar(temp2, matrixmultiply(temp1, temp1), 0.001))
    }

    test("MatrixFunc Test : Matrix Random") {
        val temp = matrixrandom(2, 2, 1, 9)
        assert(temp.size == 2)
        assert(temp.head.size == 2)
        assert(temp.map(_.min).min >= 1)
        assert(temp.map(_.max).max <= 9)
    }

    val normalizeddata = normalize(data)
    test("MatrixFunc Test : Normalization") {
        assert(arrayequal(normalizeddata._1, Array(1.0,2.2,3.0)))
        assert(arrayequal(arrrint4(normalizeddata._2), Array(0.6325, 0.7483, 2.4495)))
        assert(arrayequal(arrrint4(normalizeddata._3(0)), Array(0.0, -0.2673, -0.8165)))
    }

    test("MatrixFunc Test : De-Normalization") {
        val denormalizeddata = denormalize(normalizeddata._3, normalizeddata._1, normalizeddata._2)
        assert(arrayequal(denormalizeddata(0), Array(1.0,2.0,1.0)))
    }

    val covariancedata = covariance(data)
    test("MatrixFunc Test : Covariance") {
        assert(arrayequal(covariancedata(0), Array(0.4,-0.4,1.0)))
    }

    test("MatrixFunc Test : Sub Matrix") {
        val submatrixdata = submatrix(data, 0, 0)
        assert(arrayequal(submatrixdata(0), Array(1.0, 6.0)))
        assert(submatrix(data, 5, 6) == null)
    }

    test("MatrixFunc Test : Determinant") {
        val determinantdata = determinant(covariancedata)
        assert(rint4(determinantdata) == 0.16)
        assert(determinant(Array(Array(0, 1), Array(1, 0))).abs < 1e-8)
    }

    val inversedata = inverse(covariancedata)
    test("MatrixFunc Test : Inverse") {
        assert(arrayequal(arrrint4(inversedata(0)), Array(8.75, 6.25, 0.0)))
        assert(inverse(Array(Array(0, 1), Array(1, 0))).map(_.sum).sum > 1E12)
    }

    test("MatrixFunc Test : Mahalanobis ^ 2") {
        val mahalanobisdistance2 = mahalanobis2(data(0), normalizeddata._1, covariancedata)
        assert(rint4(mahalanobisdistance2) == 2.75)
    }

    test("MatrixFunc Test : Gaussian Probability") {
        val gaussianprobabilitydata = gaussianprobability(data(0), normalizeddata._1, covariancedata)
        assert(rint4(gaussianprobabilitydata) == 0.0401)
    }

    val dotdata = matrixdot(covariancedata, inversedata)
    test("MatrixFunc Test : Matrix Dot") {
        assert(arrayequal(arrrint4(matrixaccumulate(dotdata)), Array(1.0, 1.0, 1.0)))
    }

    test("MatrixFunc Test : Gradient Descent") {
        val temp = gradientDescent(Array(Array(1.0, 1.0), Array(1.0, 1.0)), Array(1.0, 1.0), 0.1, 100)
        assert(arraysimilar(Array(0.5, 0.5), temp, 0.1))
    }
}
