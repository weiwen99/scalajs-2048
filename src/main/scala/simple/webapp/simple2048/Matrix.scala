package simple.webapp.simple2048

object Matrix {

  def transfer(command: Command, matrix: Vector[Vector[Int]]): Vector[Vector[Int]] =
    command match {
      case Command.Up    => upTransferM(matrix)
      case Command.Down  => downTransferM(matrix)
      case Command.Left  => leftTransferM(matrix)
      case Command.Right => rightTransferM(matrix)
      case _             => matrix
    }

  def leftTransfer(xs: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def f(xs: List[Int], k: Int, z: List[Int]): List[Int] = {
      xs match {
        case Nil               => if (k == 0) z.reverse else (k :: z).reverse
        case 0 :: ys           => f(ys, k, z)
        case x :: ys if x == k => f(ys, 0, (x + k) :: z)
        case x :: ys if k == 0 => f(ys, x, z)
        case x :: ys           => f(ys, x, k :: z)
      }
    }
    val zs = f(xs, 0, Nil)
    val n  = xs.length
    val m  = zs.length
    zs ++ List.fill(n - m)(0)
  }

  def leftTransfer(xs: Vector[Int]): Vector[Int] = leftTransfer(xs.toList).toVector

  def leftTransferM(xs: Vector[Vector[Int]]): Vector[Vector[Int]]  = xs.map(leftTransfer)
  def rightTransferM(xs: Vector[Vector[Int]]): Vector[Vector[Int]] = xs.map(_.reverse).map(leftTransfer).map(_.reverse)
  def upTransferM(xs: Vector[Vector[Int]]): Vector[Vector[Int]] =
    leftTransferM(xs.rotationAntiClockwise).rotationClockwise
  def downTransferM(xs: Vector[Vector[Int]]): Vector[Vector[Int]] =
    leftTransferM(xs.rotationClockwise).rotationAntiClockwise

  implicit class MatrixImplicit(xss: Vector[Vector[Int]]) {
    def rotationAntiClockwise: Vector[Vector[Int]] = {
      val n = xss.length
      (0 until n).toVector.map { i =>
        xss.map(_(n - i - 1))
      }
    }
    def rotationClockwise: Vector[Vector[Int]] = {
      val n = xss.length
      (0 until n).toVector.map { i =>
        xss.map(_(i)).reverse
      }
    }
    def isZero: Boolean = xss.forall(_.forall(_ == 0))
  }
}
