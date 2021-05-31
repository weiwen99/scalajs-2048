package simple.webapp.simple2048

import org.scalajs.dom.Element

object Model {

  type Id         = Int
  type DataMatrix = Vector[Vector[Int]]
  type UiMatrix   = Vector[Vector[UiElement]]

  final case class UiElement(
    svg: Element,
    text: Element
  )

  final case class Coordinate(x: Int, y: Int)

  object Block {
    def empty: Block = Block(0, 0)
  }

  final case class Block(
    id: Id,
    // coordinate: Coordinate,
    value: Int
  )

  final case class State(
    ui: UiMatrix,
    matrix: Vector[Vector[Block]]
  )

  implicit val blockTransfer: Transformable[Block] = new Transformable[Block] {
    override def id(a: Block): Id                 = a.id
    override def empty: Block                     = Block(0, 0)
    override def ===(a: Block, b: Block): Boolean = a.value == b.value
    override def ++(a: Block, b: Block): Block    = b.copy(value = a.value + b.value)
  }

  implicit val intTransfer: Transformable[Int] = new Transformable[Int] {
    override def id(a: Int): Id               = a
    override def empty: Int                   = 0
    override def ===(a: Int, b: Int): Boolean = a == b
    override def ++(a: Int, b: Int): Int      = a + b
  }

}
