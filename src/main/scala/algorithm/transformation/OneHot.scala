// Wei Chen - OneHot
// 2018-11-07

package com.scalaml.algorithm

class OneHot(
    // name, type
    var table: Array[(String, String)],
    // name, cname list
    var categoryTable: Map[String, Array[String]] = Map[String, Array[String]](),
    // name, category (null for number)
    var decodeTable: Array[(String, String)] = Array[(String, String)]()
) {
    def encode(data: Array[Array[String]]): Array[Array[Double]] = {
        if(data.head.size == table.size) {
            // Pre-process
            data.foreach { row =>
                row.zip(table).foreach { case (str, (name, ctype)) =>
                    if(ctype == "category") {
                        val arr = categoryTable.getOrElse(name, Array[String]())
                        if(!arr.contains(str)) categoryTable += name -> (arr :+ str)
                    }
                }
            }
            decodeTable = table.flatMap { case (name, ctype) =>
                if(categoryTable.contains(name)) categoryTable(name).map((name, _))
                else Array((name, null: String))
            }
            // One-Hot
            data.map { row =>
                row.zip(table).flatMap { case (str, (name, ctype)) =>
                    if(categoryTable.contains(name)) {
                        val arr = new Array[Double](categoryTable(name).size)
                        arr(categoryTable(name).indexOf(str)) = 1.0
                        arr
                    } else {
                        Array(str.toDouble)
                    }
                }
            }
        } else null
    }

    def decode(data: Array[Array[Double]]): Array[Array[String]] = {
        if(decodeTable.size == data.head.size) {
            data.map { row =>
                row.zip(decodeTable).flatMap { case (value, (name, cname)) =>
                    if(cname == null) Array(value.toString)
                    else if(value < 0.5) Array[String]()
                    else Array(cname)
                }
            }
        } else null
    }
}