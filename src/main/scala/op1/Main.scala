package op1

import scala.math.*
import op1.VectorUtils.*
import op1.MatrixUtils.*

enum SearchMethodType:
  case Bisection, GoldenRatio, Fibonacci, PerCoordinate, Gradient, ConjugateGradient, NewtonRaphson, InternalPenalty, ExternalPenalty, None

object SearchMethodType:
  def asString(method: SearchMethodType): String = method match
    case Bisection     => "Bisection"
    case GoldenRatio   => "Golden ratio"
    case Fibonacci     => "Fibonacci"
    case PerCoordinate => "Per-coordinate descent"
    case Gradient          => "Gradient descent"
    case ConjugateGradient => "Conjugate gradient"
    case NewtonRaphson     => "Newton–Raphson"
    case InternalPenalty   => "Internal penalty"
    case ExternalPenalty   => "External penalty"
    case None          => "None"


final case class SearchResult(methodType: SearchMethodType, result: Any, accuracy: Double, iterations: Long, functionProbes: Long):
  override def toString: String =
    s"${SearchMethodType.asString(methodType)}: " +
      s"result = $result, accuracy = $accuracy, iterations = $iterations, functionProbes = $functionProbes"

final class OneDimensionalRunner(func: Double => Double, leftBound: Double, rightBound: Double, eps: Double, maxIter: Int):

  def runAll(): Seq[SearchResult] =
    Seq(runBisection(), runGoldenRatio(), runFibonacci())

  private def runBisection(): SearchResult =
    val (minPoint, accuracy, iter, numCalls) =
      OneDimensional.dichotomy(func, leftBound, rightBound, eps, maxIter)
    SearchResult(SearchMethodType.Bisection, minPoint, accuracy, iter, numCalls)

  private def runGoldenRatio(): SearchResult =
    val (minPoint, accuracy, iter, numCalls) =
      OneDimensional.goldenSection(func, leftBound, rightBound, eps, maxIter)
    SearchResult(SearchMethodType.GoldenRatio, minPoint, accuracy, iter, numCalls)

  private def runFibonacci(): SearchResult =
    val (minPoint, accuracy, iter, numCalls) =
      OneDimensional.fibonacciSearch(func, leftBound, rightBound, eps)
    SearchResult(SearchMethodType.Fibonacci, minPoint, accuracy, iter, numCalls)

final class MultiDimensionalRunner(func: Vec => Double, leftBound: Vec, rightBound: Vec, startPoint: Vec, eps: Double, maxIter: Int, constraints: Seq[Vec => Double] = Seq(), eqConstraints: Seq[Vec => Double] = Seq()):

  def runAll(): Seq[SearchResult] =
    Seq(
      runBisection(),
      runGoldenRatio(),
      runFibonacci(),
      runPerCoord(),
      runGradientDescent(),
      runConjugateGradient(),
      runNewtonRaphson(),
      runInternalPenalty(),
      runExternalPenalty()
    )

  private def runBisection(): SearchResult =
    val (vec, acc, iter, calls) = MultiDimensional.dichotomy(func, leftBound, rightBound, eps, maxIter)
    SearchResult(SearchMethodType.Bisection, vec, acc, iter, calls)

  private def runGoldenRatio(): SearchResult =
    val (vec, acc, iter, calls) = MultiDimensional.goldenSection(func, leftBound, rightBound, eps, maxIter)
    SearchResult(SearchMethodType.GoldenRatio, vec, acc, iter, calls)

  private def runFibonacci(): SearchResult =
    val (vec, acc, iter, calls) = MultiDimensional.fibonacciSearch(func, leftBound, rightBound, eps)
    SearchResult(SearchMethodType.Fibonacci, vec, acc, iter, calls)

  private def runPerCoord(): SearchResult =
    val (vec, acc, iter, calls) = MultiDimensional.perCordDescend(func, startPoint, eps, maxIter)
    SearchResult(SearchMethodType.PerCoordinate, vec, acc, iter, calls)

  private def runGradientDescent(): SearchResult =
    val (vec, acc, iter, calls) = MultiDimensionalHigher.gradientDescent(func, startPoint, eps, maxIter)
    SearchResult(SearchMethodType.Gradient, vec, acc, iter, calls)

  private def runConjugateGradient(): SearchResult =
    val (vec, acc, iter, calls) = MultiDimensionalHigher.conjGradientDescent(func, startPoint, eps, maxIter)
    SearchResult(SearchMethodType.ConjugateGradient, vec, acc, iter, calls)

  private def runNewtonRaphson(): SearchResult =
    val (vec, acc, iter, calls) = MultiDimensionalHigher.newtonRaphson(func, startPoint, eps, maxIter)
    SearchResult(SearchMethodType.NewtonRaphson, vec, acc, iter, calls)

  private def runInternalPenalty(): SearchResult =
    val (vec, acc, iter, calls) = MultiDimensionalHigher.internalPenalty(func, constraints, startPoint, eps, maxIter)
    SearchResult(SearchMethodType.InternalPenalty, vec, acc, iter, calls)

  private def runExternalPenalty(): SearchResult =
    val (vec, acc, iter, calls) = MultiDimensionalHigher.externalPenalty(func, constraints, eqConstraints, startPoint, eps, maxIter)
    SearchResult(SearchMethodType.ExternalPenalty, vec, acc, iter, calls)

object TestFunc:
  val func1D: Double => Double = x => pow(x - 3.0, 2) + 1
  val funcND: Vec => Double = v => (v(0) - 5.0) * v(0) + (v(1) - 3.0) * v(1)

val constraints: Seq[Vec => Double] = Seq(
  v => 2.0 - v(0),
  v => 1.0 - v(1),
  v => v(0) + v(1) - 7.0
)

val equalityConstraints: Seq[Vec => Double] = Seq(
  v => v(0) + v(1) - 6.0
)

@main def runOptimization(): Unit =
  val c: Vec = Vector(1.0, 0.0, 0.0)
  val seek = false
  val b: Vec = Vector(-2.0, 20.0)
  val maxIters = 10000

  val A: Matrix = Array(
    Array(-1.0, -1.0, -1.0),
    Array(4.0, 5.0, 5.0)
  )

  val signs: Vector[Sign] = Vector(
    Sign.More, // -x - y - z >= -2
    Sign.Less // 4x + 5y + 5z <= 20
  )

  val simplex = new Simplex(
    aInit = A,
    cInit = c,
    bInit = b,
    inequalitiesInit = signs
  )

  simplex.setMaxIterations(maxIters)

  val result =
    if seek then
      simplex.solve(SimplexProblemType.Min)
    else
      simplex.solve(SimplexProblemType.Max)

  println("\nSimplex Result:")

  result match
    case Some(v) =>
      val value =
        c.zip(v).map { case (ci, xi) => ci * xi }.sum

      println(s"value      = $value")
      println(s"result     = $v")
      println(s"iterations = ${simplex.iterations}")

    case None =>
      println("No solution")