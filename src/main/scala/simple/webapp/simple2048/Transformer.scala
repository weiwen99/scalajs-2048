package simple.webapp.simple2048

import simple.webapp.simple2048.Model.{Block, Coordinate, Id}

trait Transformable[A] {
  def id(a: A): Id
  def empty: A
  def ===(a: A, b: A): Boolean
  def ++(a: A, b: A): A
  def isEmpty(a: A): Boolean = ===(a, empty)
}

object Transformer {
  implicit class ImplicitTransfer[A: Transformable](a: A) {
    private[this] val tt: Transformable[A] = implicitly[Transformable[A]]
    def id: Id                             = tt.id(a)
    def ===(b: A): Boolean                 = tt.===(a, b)
    def |+|(b: A): A                       = tt.++(a, b)
    def isEmpty: Boolean                   = tt.isEmpty(a)
  }
  implicit class TransferImplicit[A: Transformable](xss: Vector[Vector[A]]) {
    def rotationAntiClockwise: Vector[Vector[A]] = {
      val n = xss.length
      (0 until n).toVector.map { i =>
        xss.map(_(n - i - 1))
      }
    }
    def rotationClockwise: Vector[Vector[A]] = {
      val n = xss.length
      (0 until n).toVector.map { i =>
        xss.map(_(i)).reverse
      }
    }
    def isZero: Boolean = xss.forall(_.forall(implicitly[Transformable[A]].isEmpty))
  }
}

class Transformer[A: Transformable] {

  import Transformer._

  private val tt   = implicitly[Transformable[A]]
  private val zero = tt.empty

  def leftTransfer(xs: List[A]): (List[A], List[(Id, Id)]) = {
    @scala.annotation.tailrec
    def f(xs: List[A], k: A, z: List[A], m: List[(Id, Id)]): (List[A], List[(Id, Id)]) = {
      xs match {
        case Nil                        => if (tt.===(k, zero)) (z.reverse, m) else ((k :: z).reverse, m)
        case x :: ys if tt.===(x, zero) => f(ys, k, z, m)
        case x :: ys if tt.===(x, k)    => f(ys, zero, tt.++(x, k) :: z, (tt.id(x), tt.id(k)) :: m)
        case x :: ys if tt.===(k, zero) => f(ys, x, z, m)
        case x :: ys                    => f(ys, x, k :: z, m)
      }
    }
    val (zs, ms) = f(xs, zero, Nil, Nil)
    val n        = xs.length
    val m        = zs.length
    val rs       = zs ++ List.fill(n - m)(zero)
    val mm = rs.zip(xs).collect {
      case (x, y) if x != tt.empty =>
        tt.id(x) -> tt.id(y)
    }
    val mms = ms.flatMap { m =>
      mm.find(_._1 == m._2).map(x => m._1 -> x._2)
    }
    (rs, mm ++ mms)
  }

  def leftTransfer(xs: Vector[A]): (Vector[A], Vector[(Id, Id)]) = {
    val (rs, ms) = leftTransfer(xs.toList)
    (rs.toVector, ms.toVector)
  }

  def leftTransferM(xs: Vector[Vector[A]]): (Vector[Vector[A]], Vector[(Id, Id)]) = {
    val rs: Vector[(Vector[A], Vector[(Id, Id)])] = xs.map(leftTransfer)
    (rs.map(_._1), rs.flatMap(_._2))
  }

  def rightTransferM(xs: Vector[Vector[A]]): (Vector[Vector[A]], Vector[(Id, Id)]) = {
    val rr: Vector[(Vector[A], Vector[(Id, Id)])] = xs.map(_.reverse).map(leftTransfer)
    (rr.map(_._1.reverse), rr.flatMap(_._2))
  }

  def upTransferM(xs: Vector[Vector[A]]): (Vector[Vector[A]], Vector[(Id, Id)]) = {
    val rs = leftTransferM(xs.rotationAntiClockwise)
    (rs._1.rotationClockwise, rs._2)
  }

  def downTransferM(xs: Vector[Vector[A]]): (Vector[Vector[A]], Vector[(Id, Id)]) = {
    val rs = leftTransferM(xs.rotationClockwise)
    (rs._1.rotationAntiClockwise, rs._2)
  }

}

class TransformerTest extends App {
  implicit val tt: Transformable[Block] = new Transformable[Block] {
    override def id(a: Block): Id                 = a.id
    override def empty: Block                     = Block(0, 0)
    override def ===(a: Block, b: Block): Boolean = a.value == b.value
    override def ++(a: Block, b: Block): Block    = b.copy(value = a.value + b.value)
  }

  val ts = new Transformer[Block]

  val empty = tt.empty

  val xs1 = List(0, 2, 2, 0, 4, 8, 0, 8, 0).zipWithIndex.map {
    case (i, i1) =>
      Block(i1 + 1, i)
  }

  println(xs1)

  val (rs, ms) = ts.leftTransfer(xs1)
  println(rs)
  ms.sortBy(_._1).foreach(println)
}
