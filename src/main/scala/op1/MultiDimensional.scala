package op1

import op1.VectorUtils._

object MultiDimensional:

  def dichotomy(func: Vec => Double, left0: Vec, right0: Vec, eps: Double, maxIter: Int): (Vec, Double, Int, Int) =
    var leftBound = copyVec(left0)
    var rightBound = copyVec(right0)
    var lastIndex = 0

    for
      index <- 0 until maxIter
      if distance(rightBound, leftBound) >= 2 * eps
    do
      lastIndex = index
      val middlePoint = mul(add(leftBound, rightBound), 0.5)
      val fLeft = func(sub(middlePoint, mul(direction(leftBound, rightBound), eps / 10.0)))
      val fRight = func(add(middlePoint, mul(direction(leftBound, rightBound), eps / 10.0)))

      if fLeft > fRight then
        leftBound = middlePoint
      else
        rightBound = middlePoint

    val finalPoint = mul(add(leftBound, rightBound), 0.5)
    val accuracy = distance(rightBound, leftBound) / 2.0

    (finalPoint, accuracy, lastIndex + 1, (lastIndex + 1) * 2)
  end dichotomy


  def goldenSection(func: Vec => Double, left0: Vec, right0: Vec, eps: Double, maxIter: Int): (Vec, Double, Int, Int) =
    var leftBound = copyVec(left0)
    var rightBound = copyVec(right0)
    val goldenRatio = 0.6180339887498948
    var lastIndex = 0

    var x1 = sub(rightBound, mul(sub(rightBound, leftBound), goldenRatio))
    var x2 = add(leftBound, mul(sub(rightBound, leftBound), goldenRatio))
    var funcAtX1 = func(x1)
    var funcAtX2 = func(x2)

    for
      index <- 0 until maxIter
      if distance(rightBound, leftBound) >= 2 * eps
    do
      lastIndex = index
      if funcAtX1 <= funcAtX2 then
        rightBound = x2
        x2 = x1
        funcAtX2 = funcAtX1
        x1 = sub(rightBound, mul(sub(rightBound, leftBound), goldenRatio))
        funcAtX1 = func(x1)
      else
        leftBound = x1
        x1 = x2
        funcAtX1 = funcAtX2
        x2 = add(leftBound, mul(sub(rightBound, leftBound), goldenRatio))
        funcAtX2 = func(x2)

    val finalPoint = mul(add(leftBound, rightBound), 0.5)
    val accuracy = distance(rightBound, leftBound) / 2.0

    (finalPoint, accuracy, lastIndex + 1, lastIndex + 3)
  end goldenSection


  def fibonacciSearch(func: Vec => Double, left0: Vec, right0: Vec, eps: Double): (Vec, Double, Int, Int) =
    var leftBound = copyVec(left0)
    var rightBound = copyVec(right0)

    var fibPrev = 1.0
    var fibCurr = 1.0
    var fibNext = 0.0
    var fibCount = 0

    while fibCurr < distance(rightBound, leftBound) / eps do
      fibNext = fibPrev
      fibPrev = fibCurr
      fibCurr = fibCurr + fibNext
      fibCount += 1

    var x1 = add(leftBound, mul(sub(rightBound, leftBound), (fibCurr - fibPrev) / fibCurr))
    var x2 = add(leftBound, mul(sub(rightBound, leftBound), fibPrev / fibCurr))
    var funcAtX1 = func(x1)
    var funcAtX2 = func(x2)

    var step = 0
    var lastIndex = 0

    while step < fibCount do
      lastIndex = step
      val fibTemp = fibCurr - fibPrev
      fibCurr = fibPrev
      fibPrev = fibTemp

      if funcAtX1 > funcAtX2 then
        leftBound = x1
        x1 = x2
        funcAtX1 = funcAtX2
        x2 = add(leftBound, mul(sub(rightBound, leftBound), fibPrev / fibCurr))
        funcAtX2 = func(x2)
      else
        rightBound = x2
        x2 = x1
        funcAtX2 = funcAtX1
        x1 = add(leftBound, mul(sub(rightBound, leftBound), (fibCurr - fibPrev) / fibCurr))
        funcAtX1 = func(x1)

      step += 1

    val finalPoint = mul(add(rightBound, leftBound), 0.5)
    val accuracy = distance(rightBound, leftBound) / 2.0

    (finalPoint, accuracy, lastIndex + 1, lastIndex + 3)
  end fibonacciSearch


  def perCordDescend(func: Vec => Double, xStart: Vec, eps: Double, maxIter: Int): (Vec, Double, Int, Int) =
    var x0 = copyVec(xStart)
    var x1 = copyVec(xStart)
    val n = x0.length
    val step = 1.0
    var optCoordinatesCount = 0
    var iter = 0
    var funcCalls = 0
    var lastFibAcc = Double.MaxValue

    while iter < maxIter do
      val cord = iter % n

      x1 = copyVec(x0)
      x1 = add(x1, mul(Vector.tabulate(n)(j => if j == cord then -eps else 0.0), 1.0))
      val y0 = func(x1)
      x1 = add(x1, mul(Vector.tabulate(n)(j => if j == cord then 2.0 * eps else 0.0), 1.0))
      val y1 = func(x1)
      x1 = add(x1, mul(Vector.tabulate(n)(j => if j == cord then -eps else 0.0), 1.0))

      funcCalls += 2

      if (y0 > y1) then
        x1 = add(x1, mul(Vector.tabulate(n)(j => if j == cord then step else 0.0), 1.0))
      else
        x1 = add(x1, mul(Vector.tabulate(n)(j => if j == cord then -step else 0.0), 1.0))

      val x_i = x0(cord)
      val (newX, acc, itersFib, callsFib) = fibonacciSearch(func, x0, x1, eps)
      funcCalls += callsFib
      lastFibAcc = acc
      x0 = copyVec(newX)

      if math.abs(x0(cord) - x_i) < 2.0 * eps then
        optCoordinatesCount += 1
        if optCoordinatesCount >= n then
          return (x0, lastFibAcc, iter, funcCalls)
      else
        optCoordinatesCount = 0

      iter += 1

    (x0, lastFibAcc, iter, funcCalls)
  end perCordDescend

end MultiDimensional