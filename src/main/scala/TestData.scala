// Wei Chen - Test Data Collection
// 2016-12-1

package ght.mi
package object TestData {
    // Classification Data
    // Linear Classification Data
    val LABELED_LINEAR_DATA: Array[(Int, Array[Double])] = Array(
        (-1, Array(2,5)),
        (-1, Array(3,4)),
        (-1, Array(4,5)),
        (1, Array(5,4)),
        (1, Array(6,5)),
        (1, Array(7,4)))
    val UNLABELED_LINEAR_DATA: Array[Array[Double]] = Array(
        Array(0,4),
        Array(1,4),
        Array(8,5),
        Array(9,5))
    val LABEL_LINEAR_DATA: Array[Int] = Array(-1, -1, 1, 1)
    // Nonlinear Classification Data
    val LABELED_NONLINEAR_DATA: Array[(Int, Array[Double])] = Array(
        (1, Array(2,2)),
        (1, Array(6,6)),
        (1, Array(0,0)),
        (1, Array(8,8)),
        (2, Array(2,6)),
        (2, Array(6,2)),
        (2, Array(0,8)),
        (2, Array(8,0)))
    val UNLABELED_NONLINEAR_DATA: Array[Array[Double]] = Array(
        Array(1,1),
        Array(3,3),
        Array(5,5),
        Array(7,7),
        Array(1,7),
        Array(7,1),
        Array(3,5),
        Array(5,3))
    val LABEL_NONLINEAR_DATA: Array[Int] = Array(1, 1, 1, 1, 2, 2, 2, 2)
    // Nonlinear High Dimension Data
    val UNLABELED_LARGE_HIGH_DIM_DATA: Array[Array[Double]] = Array(
        Array(1, 1, 0, 0, 1, 0, 1, 0),
        Array(1, 0, 0, 0, 1, 0, 1, 0),
        Array(1, 1, 0, 0, 1, 0, 1, 0),
        Array(0, 0, 1, 1, 0, 1, 1, 0),
        Array(0, 0, 1, 0, 0, 1, 1, 0),
        Array(0, 0, 1, 1, 0, 1, 1, 0))
    val TARGET_LARGE_HIGH_DIM_DATA: Array[Array[Double]] = Array(
        Array(1, 0),
        Array(1, 0),
        Array(1, 0),
        Array(0, 1),
        Array(0, 1),
        Array(0, 1))
    val UNLABELED_SMALL_HIGH_DIM_DATA: Array[Array[Double]] = Array(
        Array(1, 1, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 1, 1, 0, 0, 0, 0))
    val TARGET_SMALL_HIGH_DIM_DATA: Array[Array[Double]] = Array(
        Array(1, 0),
        Array(0, 1))
    // Clustering Data
    // Tiny Clustered Data
    val UNLABELED_TINY_DATA: Array[Array[Double]] = Array(
        Array(2, 2),
        Array(1, 2),
        Array(0, 2),
        Array(2, 0),
        Array(1, 0),
        Array(0, 0))
    val LABEL_TINY_DATA: Array[Int] = Array(1, 1, 1, 2, 2, 2)
    // Small Clustered Data 
    val UNLABELED_SMALL_DATA: Array[Array[Double]] = Array(
        Array(1, 2),
        Array(1, 1),
        Array(2, 1),
        Array(2, 3),
        Array(1, 0),
        Array(2, 2),
        Array(6, 5),
        Array(6, 7),
        Array(6, 6),
        Array(5, 6),
        Array(7, 6),
        Array(6, 8))
    val LABEL_SMALL_DATA: Array[Int] = Array(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
    // Unlabeled Large Data (Classroom Data)
    val UNLABELED_LARGE_DATA: Array[Array[Double]] = Array(
        Array(0, 10),
        Array(0, 11),
        Array(0, 13),
        Array(0, 12),
        Array(0, 16),
        Array(0, 17),
        Array(1, 11),
        Array(1, 14),
        Array(1, 15),
        Array(1, 18),
        Array(2, 10),
        Array(2, 13),
        Array(2, 17),
        Array(2, 19),
        Array(3, 16),
        Array(0, 11),
        Array(0, 12),
        Array(0, 14),
        Array(0, 13),
        Array(0, 17),
        Array(0, 18),
        Array(1, 12),
        Array(1, 15),
        Array(1, 16),
        Array(1, 19),
        Array(2, 11),
        Array(2, 14),
        Array(2, 18),
        Array(2, 20),
        Array(3, 17),
        Array(6, 14),
        Array(7, 10),
        Array(7, 13),
        Array(7, 18),
        Array(7, 19),
        Array(8, 11),
        Array(8, 13),
        Array(8, 16),
        Array(8, 17),
        Array(9, 10),
        Array(9, 12),
        Array(9, 14),
        Array(9, 15),
        Array(9, 18),
        Array(9, 19),
        Array(6, 15),
        Array(7, 11),
        Array(7, 14),
        Array(7, 19),
        Array(7, 20),
        Array(8, 12),
        Array(8, 14),
        Array(8, 17),
        Array(8, 18),
        Array(9, 11),
        Array(9, 13),
        Array(9, 15),
        Array(9, 16),
        Array(9, 19),
        Array(9, 20))
    val LABEL_LARGE_DATA: Array[Int] = Array(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
}
