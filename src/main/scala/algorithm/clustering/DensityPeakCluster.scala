// Wei Chen - Alex Rodriquez Fast Cluster Science 2014
// 2015-07-08

package com.scalaml.algorithm
import com.scalaml.general.MatrixFunc._

class DensityPeakCluster() extends Clustering {
    val algoname: String = "DensityPeakCluster"
    val version: String = "0.1"
    // Density Delta Data
    var dddata = Array[(Double, Double, Int)]()
    var sd = 1.0
    var densityf = 3.0
    var deltaf = 3.0

    override def clear(): Boolean = try {
        dddata = Array[(Double, Double, Int)]()
        sd = 1.0
        densityf = 3.0
        deltaf = 3.0
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    override def config(paras: Map[String, Any]): Boolean = try {
        sd = paras.getOrElse("STANDARDDEVIATION", paras.getOrElse("standarddeviation", paras.getOrElse("sd", 1.0))).asInstanceOf[Double]
        densityf = paras.getOrElse("DENSITYFILTER", paras.getOrElse("densityfilter", paras.getOrElse("densityf", 3.0))).asInstanceOf[Double]
        deltaf = paras.getOrElse("DELTAFILTER", paras.getOrElse("deltafilter", paras.getOrElse("deltaf", 3.0))).asInstanceOf[Double]
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }
    // Density Delta export import
    def importdd(data: Array[(Double, Double, Int)]) = dddata = data
    // density(Array(Array(1.0,2.0), Array(2.0,2.0),
    //   Array(1.0,0.1), Array(0.0,0.0)))
    override def cluster(data: Array[Array[Double]]) = {

        val densitydata = data.zipWithIndex.map { l1 =>
            (l1._2, data.map { l2 =>
                Math.exp(-arrayminussquare(l1._1, l2).sum / Math.pow(sd, 2))
            }.sum, l1._1)
        }
        dddata = densitydata.map { l1 =>
            val t = l1._3
            val ld = densitydata.filter(_._2 > l1._2).map { l2 =>
                (Math.sqrt(arrayminussquare(t, l2._3).sum), l2._1)
            }
            if (ld.size > 0) {
                val ldmin = ld.minBy(_._1)
                (l1._2, ldmin._1, ldmin._2)
            } else (l1._2, t.size.toDouble * sd, -1)
        }

        val densitydelta = dddata.zipWithIndex.map { l =>
            (l._2, l._1._1, l._1._2, l._1._3)
        }.sortBy(l => l._2).reverse
        var groupdata = Array[(Int, Int, Int)]()
        var group = 0
        for (l <- densitydelta) {
            if (l._4 < 0 || (l._2 > densityf && l._3 > deltaf)) {
                group += 1
                groupdata :+= (l._1, l._4, group)
            } else groupdata :+= (l._1, l._4, 0)
        }
        var groupkeymap = groupdata.map(l => (l._1, l._3)).toMap
        for (l <- groupdata) {
            if (l._3 == 0) groupkeymap += (l._1 -> groupkeymap(l._2))
        }
        // Returned Result
        groupkeymap.toArray.sortBy(_._1).map(_._2)
    }
}
