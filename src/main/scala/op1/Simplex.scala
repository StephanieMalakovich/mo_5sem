package op1

import scala.collection.mutable
import scala.math.abs
import scala.annotation.tailrec
import VectorUtils._
import MatrixUtils._

enum Sign:
  case Equal, Less, More

enum SimplexProblemType:
  case Min, Max

final class Simplex(
                     aInit: Matrix,
                     cInit: Vec,
                     bInit: Vec,
                     inequalitiesInit: Vector[Sign]
                   ):
  require(bInit.size == inequalitiesInit.size, "b.size != inequalities.size")
  require(aInit.length == inequalitiesInit.size, "A.rows != inequalities.size")
  require(aInit.head.length == cInit.size, "A.cols != c.size")

  private var mode: SimplexProblemType = SimplexProblemType.Max
  private var maxIterations = 500
  private var lastIterations = 0
  private var eps = 1e-6

  def iterations: Int = lastIterations

  private var inequalities: mutable.Buffer[Sign] = _

  private val naturalArgs = mutable.Buffer[Int]()
  private val basisArgs = mutable.Buffer[Int]()
  private val virtualArgs = mutable.Buffer[Int]()

  private var simplexTable: Matrix = _
  private var boundsVector: Vec = _
  private val boundsMatrix: Matrix = aInit.map(_.clone)
  private val pricesVector: Vec = cInit

  def setMaxIterations(v: Int): Unit =
    maxIterations = math.max(10, math.min(v, 10_000))

  private def rowsCount = simplexTable.length
  private def colsCount = simplexTable.head.length

  def currentSolution(onlyNatural: Boolean): Vec =
    val size = if onlyNatural then pricesVector.size else colsCount - 1
    var result = Vector.fill(size)(0.0)
    for i <- basisArgs.indices do
      val id = basisArgs(i)
      if id < size then
        result = result.updated(id, simplexTable(i).last)
    result

  private def isTargetFuncModified: Boolean = virtualArgs.nonEmpty

  private def isPlanOptimal: Boolean =
    val lastRow = simplexTable(rowsCount - 1)
    val mainOptimal = lastRow.init.forall(_ >= -eps)

    if !isTargetFuncModified then return mainOptimal

    val prevRow = simplexTable(rowsCount - 2)
    naturalArgs.forall(id => prevRow(id) >= -eps)

  private def mainCol: Int =
    val row = simplexTable(rowsCount - 1)
    var best = 0.0
    var idx = -1

    for i <- 0 until colsCount - 1 do
      if row(i) < best then
        best = row(i)
        idx = i

    if isTargetFuncModified then
      val rowW = simplexTable(rowsCount - 2)
      var bestW = 0.0
      var idxW = -1
      for id <- naturalArgs do
        if rowW(id) < bestW then
          bestW = rowW(id)
          idxW = id

      if idxW != -1 then return idxW

    idx

  private def mainRow(col: Int): Int =
    var delta = Double.MaxValue
    var idx = -1
    val bCol = colsCount - 1
    val rowsN = if isTargetFuncModified then rowsCount - 2 else rowsCount - 1

    for i <- 0 until rowsN do
      val a = simplexTable(i)(col)
      if a > eps then
        val r = simplexTable(i)(bCol) / a
        if r < delta then
          delta = r
          idx = i
    idx

  private def buildVirtualBasisCol(row: Int, sign: Sign): (Int, Int) = sign match
    case Sign.Equal =>
      addColumn(row, 1.0)
      val id = colsCount - 1
      (id, id)
    case Sign.More =>
      addColumn(row, -1.0)
      addColumn(row, 1.0)
      (colsCount - 2, colsCount - 1)
    case Sign.Less =>
      addColumn(row, 1.0)
      (colsCount - 1, -1)

  private def buildSimplexTable(): Unit =
    simplexTable = boundsMatrix.map(_.clone)
    boundsVector = bInit
    inequalities = inequalitiesInit.toBuffer

    naturalArgs.clear()
    basisArgs.clear()
    virtualArgs.clear()

    for i <- boundsVector.indices do
      if boundsVector(i) < 0 then
        boundsVector = boundsVector.updated(i, -boundsVector(i))
        simplexTable(i) = simplexTable(i).map(-_)
        inequalities(i) = if inequalities(i) == Sign.Less then Sign.More else Sign.Less

    for i <- pricesVector.indices do naturalArgs += i

    for i <- inequalities.indices do
      val (nat, bas) = buildVirtualBasisCol(i, inequalities(i))
      naturalArgs += nat
      if bas != -1 then
        basisArgs += bas
        virtualArgs += bas
      else
        basisArgs += nat

    simplexTable = simplexTable.zip(boundsVector).map { case (r, b) => r :+ b }

    val deltas = pricesVector
      .map(v => if mode == SimplexProblemType.Max then -v else v)
      .padTo(colsCount, 0.0)

    simplexTable :+= deltas.toArray

    if isTargetFuncModified then
      val v = Array.fill(colsCount)(0.0)
      virtualArgs.foreach(v(_) = 1.0)
      simplexTable :+= v

  private def addColumn(row: Int, value: Double): Unit =
    simplexTable = simplexTable.zipWithIndex.map { (r, i) =>
      r :+ (if i == row then value else 0.0)
    }

  private def excludeModArgs(): Boolean =
    if !isTargetFuncModified then return false

    val wRowIdx = rowsCount - 1
    val constraintsLimit = rowsCount - 2

    @tailrec
    def eliminate(col: Int, row: Int): Unit =
      if row >= constraintsLimit then ()
      else
        val pivotVal = simplexTable(row)(col)
        if abs(pivotVal) > eps then
          val wVal = simplexTable(wRowIdx)(col)
          if abs(wVal) > eps then
            val factor = wVal / pivotVal
            simplexTable(wRowIdx) = simplexTable(wRowIdx)
              .zip(simplexTable(row))
              .map { case (w, r) => w - factor * r }

        eliminate(col, row + 1)

    for id <- virtualArgs do eliminate(id, 0)
    true

  private def validateSolution: Boolean =
    val nRows = if isTargetFuncModified then rowsCount - 2 else rowsCount - 1
    val nCols = colsCount - 1

    if isTargetFuncModified && abs(simplexTable.last(nCols)) > eps then return false

    var value = 0.0
    for i <- basisArgs.indices do
      val id = basisArgs(i)
      if id < pricesVector.size then
        value += simplexTable(i)(nCols) * pricesVector(id)

    val tableValue = simplexTable(nRows)(nCols)
    val expected = if mode == SimplexProblemType.Max then tableValue else -tableValue

    abs(value - expected) < eps

  def solve(problemType: SimplexProblemType = SimplexProblemType.Max): Option[Vec] =
    mode = problemType
    buildSimplexTable()
    excludeModArgs()

    var iter = 0
    while !isPlanOptimal && iter < maxIterations do
      iter += 1
      val col = mainCol
      if col == -1 then
        lastIterations = iter
        return if validateSolution then Some(currentSolution(onlyNatural = true)) else None

      val row = mainRow(col)
      if row == -1 then
        lastIterations = iter
        return None

      basisArgs(row) = col
      val pivot = simplexTable(row)(col)
      simplexTable(row) = simplexTable(row).map(_ / pivot)

      for i <- simplexTable.indices if i != row do
        val k = simplexTable(i)(col)
        if abs(k) > eps then
          simplexTable(i) = simplexTable(i)
            .zip(simplexTable(row))
            .map { case (x, y) => x - k * y }

    lastIterations = iter
    if validateSolution then Some(currentSolution(onlyNatural = true)) else None