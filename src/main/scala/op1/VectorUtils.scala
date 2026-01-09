package op1

object VectorUtils {
  type Vec = Vector[Double]

  def add(a: Vec, b: Vec): Vec = a.zip(b).map { case (x, y) => x + y }
  def sub(a: Vec, b: Vec): Vec = a.zip(b).map { case (x, y) => x - y }
  def mul(a: Vec, k: Double): Vec = a.map(_ * k)
  def div(a: Vec, k: Double): Vec = a.map(_ / k)
  def dot(a: Vec, b: Vec): Double = a.zip(b).map { case (x, y) => x * y }.sum
  def norm(a: Vec): Double = math.sqrt(a.map(x => x * x).sum)

  def zeroVec(n: Int): Vec = Vector.fill(n)(0.0)

  def distance(a: Vec, b: Vec): Double = norm(sub(a, b))
  def direction(a: Vec, b: Vec): Vec = {
    val d = sub(b, a)
    val len = norm(d)
    if (len == 0.0) d else div(d, len)
  }

  def copyVec(a: Vec): Vec = a.map(identity)
}
