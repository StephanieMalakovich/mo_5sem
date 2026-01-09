package op1

import scala.math.*

object OneDimensional:

  def dichotomy(func: Double => Double, left0: Double, right0: Double, eps: Double, maxIter: Int): (Double, Double, Int, Int) =
    var leftBound = left0
    var rightBound = right0
    var lastIndex = 0

    for
      index <- 0 until maxIter
      if (rightBound - leftBound) >= 2 * eps
    do
      lastIndex = index
      val middlePoint = (leftBound + rightBound) / 2.0

      if func(middlePoint - eps / 10) > func(middlePoint + eps / 10) then
        leftBound = middlePoint
      else
        rightBound = middlePoint

    val finalPoint = (leftBound + rightBound) / 2.0
    val accuracy = math.abs(rightBound - leftBound) / 2.0

    (finalPoint, accuracy, lastIndex + 1, (lastIndex + 1) * 2)
  end dichotomy

  def goldenSection(func: Double => Double, left0: Double, right0: Double, eps: Double, maxIter: Int): (Double, Double, Int, Int) =
    var leftBound = left0
    var rightBound = right0
    val goldenRatio = (sqrt(5.0) - 1.0) / 2.0
    var lastIndex = 0

    var x1 = rightBound - goldenRatio * (rightBound - leftBound)
    var x2 = leftBound + goldenRatio * (rightBound - leftBound)
    var funcAtX1 = func(x1)
    var funcAtX2 = func(x2)

    for
      index <- 0 until maxIter
      if (rightBound - leftBound) >= 2 * eps
    do
      lastIndex = index
      if funcAtX1 <= funcAtX2 then
        rightBound = x2
        x2 = x1
        funcAtX2 = funcAtX1
        x1 = rightBound - goldenRatio * (rightBound - leftBound)
        funcAtX1 = func(x1)
      else
        leftBound = x1
        x1 = x2
        funcAtX1 = funcAtX2
        x2 = leftBound + goldenRatio * (rightBound - leftBound)
        funcAtX2 = func(x2)

    val finalPoint = (leftBound + rightBound) / 2.0
    val accuracy = math.abs(rightBound - leftBound) / 2.0

    (finalPoint, accuracy, lastIndex + 1,  lastIndex + 3)
  end goldenSection

  def fibonacciSearch(func: Double => Double, left0: Double, right0: Double, eps: Double): (Double, Double, Int, Int) =
    var leftBound = math.min(left0, right0)
    var rightBound = math.max(left0, right0)

    var fibPrev = 1L
    var fibCurr = 1L
    var fibNext = 0L
    var fibCount = 0

    while (fibCurr < (rightBound - leftBound) / eps)
      fibNext = fibPrev
      fibPrev = fibCurr
      fibCurr = fibCurr + fibNext
      fibCount += 1


    var x1 = leftBound + (fibCurr - fibPrev).toDouble / fibCurr * (rightBound - leftBound)
    var x2 = leftBound + (fibPrev.toDouble / fibCurr) * (rightBound - leftBound)
    var funcAtX1 = func(x1)
    var funcAtX2 = func(x2)

    var step = 0
    var lastIndex = 0

    while (step < fibCount)
      lastIndex = step
      val fibTemp = fibCurr - fibPrev
      fibCurr = fibPrev
      fibPrev = fibTemp

      if funcAtX1 > funcAtX2 then
        leftBound = x1
        x1 = x2
        funcAtX1 = funcAtX2
        x2 = leftBound + (rightBound - leftBound) * (fibPrev.toDouble / fibCurr)
        funcAtX2 = func(x2)
      else
        rightBound = x2
        x2 = x1
        funcAtX2 = funcAtX1
        x1 = leftBound + (rightBound - leftBound) * ((fibCurr - fibPrev).toDouble / fibCurr)
        funcAtX1 = func(x1)

      step += 1

    val finalPoint = (leftBound + rightBound) / 2.0
    val accuracy = math.abs(rightBound - leftBound) / 2.0

    (finalPoint, accuracy, lastIndex + 1, lastIndex + 3)
  end fibonacciSearch

end OneDimensional
