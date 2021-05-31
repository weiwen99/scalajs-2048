package simple.webapp.simple2048

import scala.util.Random

import org.scalajs.dom
import org.scalajs.dom.raw.Element
import org.scalajs.dom.document
import Model._
import Transformer._

object SvgMain {

  val tt = new Transformer[Block]

  val random = new Random(System.currentTimeMillis())

  var svgs: Vector[Vector[(Element, Element)]] = Vector.empty

  var M: Vector[Vector[Block]] = Vector.fill(4)(Vector.fill(4)(Block.empty))

  var cursor: Int = 1

  def growth(): Vector[Vector[Block]] = {
    val xs = for {
      x <- 0 to 3
      y <- 0 to 3 if M(x)(y) === blockTransfer.empty
    } yield (x, y)
    if (xs.nonEmpty) {
      val n      = xs.length
      val (x, y) = xs(random.nextInt(n))
      val v      = if (random.nextInt(10) >= 8) 4 else 2
      val e      = Block(id = cursor, value = v)
      cursor = cursor + 1
      M = M.updated(x, M(x).updated(y, e))
    } else {}
    M
  }

  def transferByCommand(command: Command, matrix: Vector[Vector[Block]]): Vector[Vector[Block]] =
    command match {
      case Command.Up    => tt.upTransferM(matrix)._1
      case Command.Down  => tt.downTransferM(matrix)._1
      case Command.Left  => tt.leftTransferM(matrix)._1
      case Command.Right => tt.rightTransferM(matrix)._1
      case _             => matrix
    }

  def transfer(command: Command): Boolean = {
    val m = transferByCommand(command, M)
    val r = M.isZero || M != m
    M = m
    r
  }

  def render(): Unit = {
    0 to 3 foreach { x =>
      0 to 3 foreach { y =>
        svgs(x)(y)._1.setAttribute("fill", Settings.settings.colors.getOrElse(M(x)(y).value, "#ffffff"))
        svgs(x)(y)._2.textContent = if (M(x)(y) === blockTransfer.empty) "" else M(x)(y).value.toString
      }
    }
  }

  def forward(command: Command): Unit = {
    val r = transfer(command)
    if (r) {
      growth()
      render()
    } else {}
  }

  def updateSvgs(gs: Vector[Vector[(Element, Element)]]): Unit = {
    svgs = gs
  }

  def main(args: Array[String]): Unit = {
    document.addEventListener(
      "DOMContentLoaded", { (e: dom.Event) =>
        val svg: Element = SvgUI.initSvg(updateSvgs)(forward)
        EventListener.addTouchEvents(svg)(forward)
        EventListener.addKeyboardEvents(forward)
      }
    )
  }
}
