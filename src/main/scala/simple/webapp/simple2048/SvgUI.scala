package simple.webapp.simple2048

import org.scalajs.dom
import org.scalajs.dom.{Element, document}
import org.scalajs.dom.html.{Button, Div}
import simple.webapp.simple2048.Model.Block

object SvgUI {

  def initSvg(updateSvgs: Vector[Vector[(Element, Element)]] => Unit)(forward: Command => Unit): Element = {
    val agents   = List("Android", "iPhone", "SymbianOS", "Windows Phone", "iPod", "iPad")
    val isMobile = agents.exists(a => dom.window.navigator.userAgent.toLowerCase.contains(a.toLowerCase))

    // 计算合适的正方形主区域大小
    def genMainSize(w: Double, h: Double, k: Double) = {
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
    val svgs: Vector[Vector[(Element, Element)]] = for {
      y <- (0 to 3).toVector
    } yield (0 to 3).toVector.map(x => fg(x, y, 4, W))

    updateSvgs(svgs)

    document.body.appendChild(container)
    container.appendChild(svg)
    if (isMobile) {
      container.appendChild(controlPlane(W * 2 / 3)(forward))
    }
    svg
  }

  def controlPlane(height: Int)(forward: Command => Unit): Div = {
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

    def addKeyboardListener(el: Button, command: Command): Unit =
      el.addEventListener("click", { (_: dom.MouseEvent) =>
        forward(command)
      })

    addKeyboardListener(up, Command.Up)
    addKeyboardListener(down, Command.Down)
    addKeyboardListener(left, Command.Left)
    addKeyboardListener(right, Command.Right)

    container
  }
}
