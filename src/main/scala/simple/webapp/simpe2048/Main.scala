package simple.webapp.simpe2048

import scala.util.Random

import org.scalajs.dom
import org.scalajs.dom.html.{Button, Div}
import org.scalajs.dom.raw.Element
import org.scalajs.dom.document

object Main {

  val random = new Random(System.currentTimeMillis())

  var svgs: Vector[Vector[(Element, Element)]] = Vector.empty

  val F: Vector[(Int, Int)] = (0 to 3).toVector.flatMap(x => (0 to 3).toVector.map(y => (x, y)))

  var M: Vector[Vector[Int]] = Vector.fill(4)(Vector.fill(4)(0))

  def growth(): Vector[Vector[Int]] = {
    val xs = F.filter {
      case (x, y) => M(x)(y) == 0
    }
    if (xs.nonEmpty) {
      val n      = xs.length
      val (x, y) = xs(random.nextInt(n))
      val e      = if (random.nextInt(10) >= 8) 4 else 2
      M = M.updated(x, M(x).updated(y, e))
    } else {}
    M
  }

  def transfer(command: Command): Unit = {
    M = Matrix.transfer(command, M)
  }

  def render(): Unit = {
    0 to 3 foreach { x =>
      0 to 3 foreach { y =>
        svgs(x)(y)._1.setAttribute("fill", colors.getOrElse(M(x)(y), "#ffffff"))
        svgs(x)(y)._2.textContent = if (M(x)(y) == 0) "" else M(x)(y).toString
      }
    }
  }

  def forward(command: Command): Unit = {
    transfer(command)
    growth()
    render()
  }

  def main(args: Array[String]): Unit = {
    document.addEventListener(
      "DOMContentLoaded", { (e: dom.Event) =>
        val svg: Element = initSvg()
        addTouchEvnets(svg)
        addKeyboardEvents()
      }
    )
  }

  def addKeyboardEvents(): Unit = {
    document.addEventListener(
      "keydown", { (e: dom.KeyboardEvent) =>
        println(s"key: ${e.key}")
        val command = e.key match {
          case "ArrowDown" | "j"  => Command.Down
          case "ArrowUp" | "k"    => Command.Up
          case "ArrowLeft" | "h"  => Command.Left
          case "ArrowRight" | "l" => Command.Right
          case _                  => Command.Nop
        }
        if (command != Command.Nop) {
          forward(command)
        }
      }
    )
  }

  def addTouchEvnets(svg: Element): Unit = {
    val msZ = (0.0, 0.0)
    var ms  = msZ
    var mv  = false
    svg.addEventListener("touchstart", { (e: dom.TouchEvent) =>
      e.preventDefault()
      ms = (e.touches(0).clientX, e.touches(0).clientY)
    })
    svg.addEventListener("touchmove", { (e: dom.TouchEvent) =>
      mv = true
      e.preventDefault()
    })
    svg.addEventListener(
      "touchend", { (e: dom.TouchEvent) =>
        e.preventDefault()
        println(e.changedTouches.length)
        val (x0, y0) = ms
        val (x1, y1) = (e.changedTouches(0).clientX, e.changedTouches(0).clientY)
        ms = msZ
        mv = false
        if (math.abs(x1 - x0) > math.abs(y1 - y0)) {
          if (x1 > x0) {
            forward(Command.Right)
          } else {
            forward(Command.Left)
          }
        } else {
          if (y1 > y0) {
            forward(Command.Down)
          } else {
            forward(Command.Up)
          }
        }
      }
    )
  }

  val colors = Map(
    0     -> "#ecf0f1",
    2     -> "#049372",
    4     -> "#2574a9",
    8     -> "#8e44ad",
    16    -> "#e67e22",
    32    -> "#e74c3c",
    64    -> "#27ae60",
    128   -> "#f7ca18",
    256   -> "#2c3e50",
    512   -> "#d35400",
    1024  -> "#f1c40f",
    2048  -> "#8e44ad",
    4096  -> "#cf000f",
    8192  -> "#f9690e",
    16384 -> "#1e824c",
    32768 -> "#9a12b3",
    65536 -> "#9a12b3"
  )

  def initSvg(): Element = {
    val agents   = List("Android", "iPhone", "SymbianOS", "Windows Phone", "iPod", "iPad")
    val isMobile = agents.exists(a => dom.window.navigator.userAgent.toLowerCase.contains(a.toLowerCase))
    def genMainSize(w: Double, h: Double, k: Double) = {
      // 计算合适的正方形主区域大小
      val min = Math.min(h, w)
      min * (1 - 2 * k)
    }

    def getWindowSize: (Double, Double) =
      (dom.window.innerWidth, dom.window.innerHeight)

    val marginTopK = 0.1
    val windowSize = getWindowSize
    val W          = genMainSize(windowSize._1, windowSize._2, marginTopK).toInt
    val H          = W
    val ns         = "http://www.w3.org/2000/svg"
    val container  = document.createElement("div").asInstanceOf[Div]
    container.style.marginTop = "30px"
    container.style.marginLeft = "auto"
    container.style.marginRight = "auto"
    container.style.width = s"${W}px"
    container.style.height = s"${H}px"
    val svg = document.createElementNS(ns, "svg")
    svg.setAttribute("width", "100%")
    svg.setAttribute("height", "100%")
    svg.setAttribute("xmlns", ns)
    def fg(i: Int, j: Int, n: Int, w: Int): (Element, Element) = {
      val margin      = 1.5
      val d           = w / n
      val innerWidth  = d - margin * 2
      val innerHeight = innerWidth
      val g           = document.createElementNS(ns, "g")
      g.setAttribute("id", s"g${i}x${j}")
      g.setAttribute("transform", s"translate(${i * d + margin}, ${j * d + margin})")
      g.setAttribute("y", "0")
      val rect = document.createElementNS(ns, "rect")
      rect.setAttribute("id", s"r${i}x${j}")
      rect.setAttribute("width", s"$innerWidth")
      rect.setAttribute("height", s"$innerHeight")
      rect.setAttribute("fill", "#eeeeee")
      rect.setAttribute("x", "0")
      rect.setAttribute("y", "0")
      rect.setAttribute("rx", "10")
      rect.setAttribute("ry", "10")
      val text = document.createElementNS(ns, "text")
      text.setAttribute("id", s"t${i}x${j}")
      text.setAttribute("x", (innerWidth / 2).toString)
      text.setAttribute("y", (innerHeight / 1.8).toString)
      text.setAttribute("text-anchor", "middle")
      text.setAttribute("font-size", s"${innerHeight / 5}px")
      text.setAttribute("fill", "#ffffff")
      g.appendChild(rect)
      g.appendChild(text)
      svg.appendChild(g)

      (rect, text)
    }

    svgs = for {
      y <- (0 to 3).toVector
    } yield (0 to 3).toVector.map(x => fg(x, y, 4, W))

    document.body.appendChild(container)
    container.appendChild(svg)
    if (isMobile) {
      container.appendChild(controlPlane(W * 2 / 3))
    }
    svg
  }

  def controlPlane(height: Int): Div = {
    val h        = height
    val hh       = h / 3
    val fontSize = s"${h / 12}px"
    val d1       = document.createElement("div").asInstanceOf[Div]
    d1.style.marginLeft = "auto"
    d1.style.marginRight = "auto"
    d1.style.width = s"${h}px"
    d1.style.height = s"${h}px"
    def createControlButton(tag: String): Button = {
      val b = document.createElement("button").asInstanceOf[Button]
      b.textContent = tag
      b.setAttribute("id", tag.toLowerCase)
      b.style.display = "block"
      b.style.marginLeft = "auto"
      b.style.marginRight = "auto"
      b.style.width = s"${hh}px"
      b.style.height = s"${hh}px"
      b.style.backgroundColor = "#cccccc"
      b.style.outline = "none"
      b.style.borderWidth = "0"
      b.style.borderRadius = "7%"
      b.style.borderColor = "#bbbbbb"
      b.style.borderStyle = "none"
      b.style.boxShadow = "0"
      b.style.color = "#ffffff"
      b.style.fontSize = fontSize
      b.style.fontFamily = "serif"
      b
    }
    val up     = createControlButton("Up")
    val middle = document.createElement("div").asInstanceOf[Div]
    middle.style.width = "100%"
    middle.style.height = s"${hh}px"
    val left = createControlButton("Left")
    left.style.cssFloat = "left"
    val right = createControlButton("Right")
    right.style.cssFloat = "right"
    val down = createControlButton("Down")
    middle.appendChild(left)
    middle.appendChild(right)
    d1.appendChild(up)
    d1.appendChild(middle)
    d1.appendChild(down)
    val container = document.createElement("div").asInstanceOf[Div]
    container.style.marginTop = "30px"
    container.appendChild(d1)

    up.addEventListener("click", { (e: dom.MouseEvent) =>
      forward(Command.Up)
    })
    down.addEventListener("click", { (e: dom.MouseEvent) =>
      forward(Command.Down)
    })
    left.addEventListener("click", { (e: dom.MouseEvent) =>
      forward(Command.Left)
    })
    right.addEventListener("click", { (e: dom.MouseEvent) =>
      forward(Command.Right)
    })

    container
  }

}
