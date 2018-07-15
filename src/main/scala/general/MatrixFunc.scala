// Wei Chen - Matrix Function
// 2015-11-26

package com.interplanetarytech.general
package object MatrixFunc {

    val equalfunc = (v: (Double, Double)) => v._1 == v._2
    val sumfunc = (v: (Double, Double)) => v._1 + v._2
    val minusfunc = (v: (Double, Double)) => v._1 - v._2
    val minus2func = (v: (Double, Double)) => Math.pow(v._1 - v._2, 2)
    val multifunc = (v: (Double, Double)) => v._1 * v._2
    // Array Functions
    def arraysimilar(x: Array[Double], y: Array[Double], err: Double): Boolean =
        x.zip(y).forall(l => Math.abs(l._1 - l._2) < err)
    def arrayequal(x: Array[Int], y: Array[Int]): Boolean =
        x.zip(y).forall(l => l._1 == l._2)
    def arrayequal(x: Array[Double], y: Array[Double]): Boolean =
        x.zip(y).forall(equalfunc)
    def arraysum(x: Array[Double], y: Array[Double]): Array[Double] =
        (x.zip(y).map(sumfunc))
    def arrayminus(x: Array[Double], y: Array[Double]): Array[Double] =
        (x.zip(y).map(minusfunc))
    def arrayminussquare(x: Array[Double], y: Array[Double]): Array[Double] =
        (x.zip(y).map(minus2func))
    def arraymultiply(x: Array[Double], y: Array[Double]): Array[Double] =
        (x.zip(y).map(multifunc))
    def arraydevide(x: Array[Double], y: Array[Double]): Array[Double] =
        (x.zip(y).map(l => if (l._2 == 0) 0 else l._1 / l._2))
    def arrayrandom(x: Int, min_v: Double, max_v: Double): Array[Double] =
        Array.fill(x)(scala.util.Random.nextDouble()*(max_v-min_v)+min_v)
    // Matrix Functions
    def matrixsimilar(x: Array[Array[Double]], y: Array[Array[Double]], err: Double): Boolean =
        x.zip(y).forall(l => l._1.zip(l._2).forall(ll => Math.abs(ll._1 - ll._2) < err))
    def matrixequal(x: Array[Array[Double]], y: Array[Array[Double]]): Boolean =
        x.zip(y).forall(l => l._1.zip(l._2).forall(equalfunc))
    def matrixaccumulate(x: Array[Array[Double]]): Array[Double] =
        x.reduceLeft((x, y) => (x.zip(y).map(sumfunc)))
    def matrixsum(x: Array[Array[Double]], y: Array[Array[Double]]): Array[Array[Double]] =
        x.zip(y).map { case (a1, a2) => a1.zip(a2).map(sumfunc) }
    def matrixminus(x: Array[Array[Double]], y: Array[Array[Double]]): Array[Array[Double]] =
        x.zip(y).map { case (a1, a2) => a1.zip(a2).map{minusfunc} }
    def matrixmultiply(x: Array[Array[Double]], y: Array[Array[Double]]): Array[Array[Double]] =
        x.zip(y).map { case (a1, a2) => a1.zip(a2).map(multifunc) }
    def matrixrandom(x: Int, y: Int, min_v: Double, max_v: Double): Array[Array[Double]] =
        Array.fill(x)(arrayrandom(y, min_v, max_v))

    // normalize(Array(Array(1,1),Array(2,2)))
    def normalize(
        data: Array[Array[Double]]
    ): (Array[Double], Array[Double], Array[Array[Double]]) = {
        val datasize = data.size
        val avg = matrixaccumulate(data).map(_/datasize)
        val std  = matrixaccumulate(data.map{l =>
            arrayminussquare(l, avg)
        }).map(l => Math.sqrt(l/datasize))
        (avg, std, data.map{l =>
            arraydevide(arrayminus(l, avg), std)
        })
    }

    // denormalize(Array(Array(0.5,0.5)), Array(0.0,0.0), Array(1.0,1.0))
    def denormalize(
        data: Array[Array[Double]],
        avg: Array[Double],
        std: Array[Double]
    ): Array[Array[Double]] =
        data.map(d => arraysum(arraymultiply(d, std), avg))

    // Covariance
    // input data Array[Array[Double]] : n x m
    // output matrix Array[Array[Double]] : m x m
    // n : number of data point
    // m : number of feature
    def covariance(
        data: Array[Array[Double]]
    ): Array[Array[Double]] = {
        val datasize = data.size
        val featuresize = data.head.size
        val avg = matrixaccumulate(data).map(_/datasize)
        val dataminus = data.map(d => arrayminus(d, avg))
        var cov = Array.ofDim[Double](featuresize, featuresize)
        dataminus.map { d =>
            for (i <- 0 until featuresize) {
                for (j <- 0 until featuresize) {
                    cov(i)(j) = cov(i)(j) + d(i) * d(j)
                }
            }
        }
        return cov.map(d => d.map(_ / datasize))
    }

    // cut submatrix for middle high dimensional determinant calculation
    def submatrix(
        data: Array[Array[Double]],
        row: Int,
        column: Int
    ): Array[Array[Double]] = {
        val datasize = data.size
        val featuresize = data.head.size
        if (datasize > row + 1 && featuresize > column + 1) {
            var submat = Array.ofDim[Double](datasize-1, featuresize-1)
            for (i <- 0 to datasize-2) {
                for (j <- 0 to featuresize-2) {
                    val ii = if (i >= row) i + 1 else i
                    val jj = if (j >= column) j + 1 else j
                    submat(i)(j) = data(ii)(jj)
                }
            }
            return submat
        }else{
            return Array(Array(0.0))
        }
    }

    // | matrix |
    def determinant(
        data: Array[Array[Double]]
    ): Double = {
        val datasize = data.size
        val featuresize = data.head.size
        var newdata = data.clone
        var det = 1.0
        for (i <- 0 to featuresize-2) {
            if (newdata(i)(i) == 0) {
                var j = i+1
                while (j < datasize) {
                    if (newdata(j)(i) != 0) {
                        val temp = newdata(j)
                        newdata(j) = newdata(i)
                        newdata(i) = temp
                        det = -det
                        j = datasize
                    }
                    j += 1
                }
            }
            val arr = newdata(i)
            val arrhead = arr(i)
            det *= arrhead
            for (j <- i+1 until datasize) {
                val newhead = newdata(j)(i)
                if (newhead != 0) {
                    newdata(j) = arrayminus(newdata(j),
                        arr.map(_ * newhead / arrhead))
                }
            }
        }
        det *= newdata(featuresize-1)(featuresize-1)
        return (if (Math.abs(det) < 1E-12) 1E-12 else det)
    }

    // inverse matrix
    def inverse(
        data: Array[Array[Double]]
    ): Array[Array[Double]] = {
        val datasize = data.size
        val featuresize = data.head.size
        var newdata = data.clone
        var resultdata = Array.ofDim[Double](featuresize, featuresize)
        for (i <- 0 until featuresize) {
            resultdata(i)(i) = 1.0
        }
        for (i <- 0 until featuresize) {
            if (newdata(i)(i) == 0) {
                var j = i+1
                while (j < datasize) {
                    if (newdata(j)(i) != 0) {
                        val temp1 = newdata(j)
                        newdata(j) = newdata(i)
                        newdata(i) = temp1
                        val temp2 = resultdata(j)
                        resultdata(j) = resultdata(i)
                        resultdata(i) = temp2
                        j = datasize
                    }
                    j += 1
                }
            }
            val arr1 = newdata(i)
            val arrhead = if (Math.abs(arr1(i)) < 1E-12) 1E-12 else arr1(i)
            val arr2 = resultdata(i)
            for (j <- 0 until datasize) {
                if (j == i) {
                    newdata(j) = newdata(j).map(_ / arrhead)
                    resultdata(j) = resultdata(j).map(_ / arrhead)
                } else {
                    val newhead = newdata(j)(i)
                    if (newhead != 0) {
                        newdata(j) = arrayminus(newdata(j), arr1.map(_ * newhead / arrhead))
                        resultdata(j) = arrayminus(resultdata(j), arr2.map(_ * newhead / arrhead))
                    }
                }
            }
        }
        return resultdata
    }

    // mahalanobis distance ^2
    def mahalanobis2(
        x: Array[Double],
        m: Array[Double],
        s: Array[Array[Double]]
    ): Double = {
        val featuresize = x.size
        val d = arrayminus(x, m)
        val inv = inverse(s)
        var dist = 0.0
        for (i <- 0 until featuresize) {
            for (j <- 0 until featuresize) {
                dist += d(i) * d(j) * inv(i)(j)
            }
        }
        return dist
    }

    // gaussian probability of a data point
    def gaussianprobability(
        x: Array[Double],
        m: Array[Double],
        s: Array[Array[Double]]
    ): Double =
        Math.exp(-mahalanobis2(x, m, s) / 2) / Math.sqrt(Math.pow(2 * Math.PI, x.size) * determinant(s))

    // matrix x matrix
    def matrixdot(
        x: Array[Array[Double]],
        y: Array[Array[Double]]
    ): Array[Array[Double]] = {
        val rows = x.size
        val columnmatch = x.head.size
        val rowmatch = y.size
        val columns = y.head.size
        var m = Array.ofDim[Double](rows, columns)
        if (columnmatch == rowmatch) {
            for (i <- 0 until rows) {
                for (j <- 0 until columns) {
                    var a = 0.0
                    for (k <- 0 until columnmatch) {
                        a += x(i)(k) * y(k)(j)
                    }
                    m(i)(j) = a
                }
            }
        }
        return m
    }
}

