package op1

import scala.annotation.tailrec
import VectorUtils._
import MatrixUtils._
import op1.MultiDimensional.fibonacciSearch

object MultiDimensionalHigher:

  def gradient(func: Vec => Double, x: Vec, eps: Double): Vec =
    Vector.tabulate(x.length) { i =>
      val xh = x.updated(i, x(i) + eps)
      (func(xh) - func(x)) / eps
    }
  end gradient

  def hessian(func: Vec => Double, x: Vec, eps: Double): Matrix =
    val n = x.length
    Array.tabulate(n, n) { (i, j) =>
      val xi = x.updated(i, x(i) + eps)
      val xj = x.updated(j, x(j) + eps)
      val xij = x.indices.map { k =>
        x(k) +
          (if k == i then eps else 0.0) +
          (if k == j then eps else 0.0)
      }.toVector

      (func(xij) - func(xi) - func(xj) + func(x)) / (eps * eps)
    }
  end hessian

  def gradientDescent(func: Vec => Double, xStart: Vec, eps: Double, maxIters: Int): (Vec, Double, Int, Int) =

    var x_i = copyVec(xStart)
    var x_i_1 = zeroVec(xStart.length)

    var totalCalls = 0
    var accuracy = Double.PositiveInfinity
    val step = 1.0

    @tailrec
    def loop(iter: Int): (Vec, Double, Int, Int) =
      if iter > maxIters then
        (x_i, accuracy, iter, totalCalls)
      else
        val grad = gradient(func, x_i, eps)
        totalCalls += 2 * x_i.length

        x_i_1 = sub(x_i, mul(grad, step))

        val (result, acc, _, tCalls) = fibonacciSearch(func, x_i, x_i_1, eps)
        totalCalls += tCalls
        accuracy = math.min(accuracy, acc)

        if distance(result, x_i) < 2 * eps then
          (result, accuracy, iter, totalCalls)
        else
          x_i = result
          loop(iter + 1)

    loop(0)
  end gradientDescent

  def conjGradientDescent(func: Vec => Double, xStart: Vec, eps: Double, maxIters: Int): (Vec, Double, Int, Int) =

    var x_i = copyVec(xStart)
    var x_i_1 = zeroVec(xStart.length)
    var gradient_prev = mul(gradient(func, x_i, eps), -1.0)

    var totalCalls = 2 * x_i.length
    var accuracy = Double.PositiveInfinity
    val step = 1.0

    @tailrec
    def loop(iter: Int): (Vec, Double, Int, Int) =
      if iter > maxIters then
        (x_i, accuracy, iter, totalCalls)
      else
        x_i_1 = add(x_i, mul(gradient_prev, step))
        val (result, acc, _, tCalls) = fibonacciSearch(func, x_i, x_i_1, eps)
        totalCalls += tCalls
        accuracy = math.min(accuracy, acc)

        x_i_1 = result

        var gradient_next = gradient(func, x_i_1, eps)
        totalCalls += 2 * x_i.length

        val omega = dot(gradient_next, gradient_next) / dot(gradient_prev, gradient_prev)
        gradient_next = sub(mul(gradient_prev, omega), gradient_next)

        if distance(x_i_1, x_i) < 2 * eps then
          (x_i_1, accuracy, iter, totalCalls)
        else
          x_i = x_i_1
          gradient_prev = gradient_next
          loop(iter + 1)

    loop(0)
  end conjGradientDescent

  def newtonRaphson(func: Vec => Double, xStart: Vec, eps: Double, maxIters: Int): (Vec, Double, Int, Int) =

    var x_i = copyVec(xStart)
    var x_i_1 = zeroVec(xStart.length)

    var grad = zeroVec(xStart.length)
    var hess = zeroMat(xStart.length)
    var hessianInv = zeroMat(xStart.length)

    var iterations = 0
    var totalCalls = 0
    var accuracy = Double.PositiveInfinity

    @tailrec
    def loop(iter: Int): (Vec, Double, Int, Int) =
      if iter > maxIters then
        (x_i, accuracy, iter, totalCalls)
      else
        grad = gradient(func, x_i, eps)
        hess = hessian(func, x_i, eps)
        hessianInv = invertMatrix(hess)

        x_i_1 = sub(x_i, mulMatrixVector(hessianInv, grad))

        totalCalls += 2 * x_i.length + x_i.length * x_i.length

        val dist = distance(x_i_1, x_i)
        accuracy = math.min(accuracy, dist)

        if dist < 2 * eps then
          (x_i_1, accuracy, iter, totalCalls)
        else
          x_i = x_i_1
          loop(iter + 1)

    loop(0)
  end newtonRaphson

  def internalPenalty(func: Vec => Double, constraints: Seq[Vec => Double], xStart: Vec, eps: Double, maxIters: Int): (Vec, Double, Int, Int) =

    var x_i = copyVec(xStart)
    var x_i_1 = zeroVec(xStart.length)
    var step = 1.0
    var totalProbes = 0
    var accuracy = Double.PositiveInfinity

    @tailrec
    def loop(iter: Int): (Vec, Double, Int, Int) =
      if iter >= maxIters then
        (x_i, distance(x_i, x_i), iter, totalProbes)
      else
        val penaltyFunction: Vec => Double = x =>
          var penalty = 0.0
          var k = 0
          while k < constraints.length do
            val fi = constraints(k)(x)
            penalty += step / (-fi)
            k += 1
          func(x) + penalty

        val (result, acc, _, tCalls) = gradientDescent(penaltyFunction, x_i, eps, maxIters)
        x_i_1 = result
        totalProbes += tCalls
        step *= 0.5

        val dist = distance(x_i_1, x_i)
        if dist < 2 * eps then
          (x_i_1, dist, iter, totalProbes)
        else
          x_i = x_i_1
          loop(iter + 1)

    loop(0)
  end internalPenalty

  def externalPenalty(func: Vec => Double, constraints: Seq[Vec => Double], eqConstraints: Seq[Vec => Double], xStart: Vec, eps: Double, maxIters: Int): (Vec, Double, Int, Int) =

    var x_i = copyVec(xStart)
    var x_i_1 = zeroVec(xStart.length)
    var step = 1.0
    var totalProbes = 0
    var accuracy = Double.PositiveInfinity

    @tailrec
    def loop(iter: Int): (Vec, Double, Int, Int) =
      if iter >= maxIters then
        (x_i, distance(x_i, x_i), iter, totalProbes)
      else
        val penaltyFunction: Vec => Double = x =>
          var penalty = 0.0

          var k = 0
          while k < constraints.length do
            val  violation = math.max(0.0, constraints(k)(x))
            penalty += step * violation * violation
            k += 1

          k = 0
          while k < eqConstraints.length do
            val hj = eqConstraints(k)(x)
            penalty += step * hj * hj
            k += 1

          func(x) + penalty

        val (result, acc, _, tCalls) = gradientDescent(penaltyFunction, x_i, eps, maxIters)
        x_i_1 = result
        totalProbes += tCalls

        step *= 0.5

        val dist = distance(x_i_1, x_i)
        if dist < 2 * eps then
          (x_i_1, dist, iter, totalProbes)
        else
          x_i = x_i_1
          loop(iter + 1)

    loop(0)
  end externalPenalty

end MultiDimensionalHigher