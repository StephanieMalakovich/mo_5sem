package op1

object MatrixUtils:

  type Matrix = Array[Array[Double]]

  def copyMatrix(m: Matrix): Matrix =
    m.map(_.clone)

  def identityMatrix(n: Int): Matrix =
    Array.tabulate(n, n)((i, j) => if i == j then 1.0 else 0.0)

  def addMatrices(a: Matrix, b: Matrix): Matrix =
    Array.tabulate(a.length, a.length)((i, j) => a(i)(j) + b(i)(j))

  def mulMatrix(a: Matrix, k: Double): Matrix =
    a.map(_.map(_ * k))

  def mulMatrixVector(a: Matrix, v: Vector[Double]): Vector[Double] =
    a.map(row => row.zip(v).map(_ * _).sum).toVector

  def zeroMat(n: Int): Matrix = Array.fill(n, n)(0.0)
  
  def invertMatrix(a: Matrix): Matrix =
    val n = a.length
    val aug = Array.ofDim[Double](n, 2 * n)

    for i <- 0 until n do
      for j <- 0 until n do aug(i)(j) = a(i)(j)
      for j <- 0 until n do aug(i)(j + n) = if i == j then 1.0 else 0.0

    for i <- 0 until n do
      var pivot = aug(i)(i)
      if math.abs(pivot) < 1e-12 then
        throw new RuntimeException("Matrix not invertible")

      for j <- 0 until 2 * n do aug(i)(j) /= pivot

      for k <- 0 until n do
        if k != i then
          val f = aug(k)(i)
          for j <- 0 until 2 * n do
            aug(k)(j) -= f * aug(i)(j)

    val inv = Array.ofDim[Double](n, n)
    for i <- 0 until n do
      for j <- 0 until n do inv(i)(j) = aug(i)(j + n)

    inv
  end invertMatrix
  

end MatrixUtils


