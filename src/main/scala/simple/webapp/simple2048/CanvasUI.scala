package simple.webapp.simple2048

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.{Canvas, Div}

object CanvasUI {

  // 计算合适的正方形主区域大小
  def genMainSize(w: Double, h: Double, k: Double): Double = Math.min(h, w) * (1 - 2 * k)

  def getWindowSize: (Double, Double) = (dom.window.innerWidth, dom.window.innerHeight)

  def drawRect(ctx: dom.CanvasRenderingContext2D, x: Double, y: Double, w: Double, h: Double, radius: Double): Unit = {
    val r = x + w;
    val b = y + h;
    ctx.beginPath()
    ctx.lineWidth = 0
    ctx.moveTo(x + radius, y)
    ctx.lineTo(r - radius, y)
    ctx.quadraticCurveTo(r, y, r, y + radius)
    ctx.lineTo(r, y + h - radius)
    ctx.quadraticCurveTo(r, b, r - radius, b)
    ctx.lineTo(x + radius, b)
    ctx.quadraticCurveTo(x, b, x, b - radius)
    ctx.lineTo(x, y + radius)
    ctx.quadraticCurveTo(x, y, x + radius, y)
    ctx.fillStyle = "#049372"
    ctx.fill()
    println((x, y, w, h, radius))
  }

  def init(): Div = {
    val marginTopK = 0.1
    val windowSize = getWindowSize
    val W          = genMainSize(windowSize._1, windowSize._2, marginTopK).toInt
    val H          = W
    val container  = document.createElement("div").asInstanceOf[Div]
    container.style.marginTop = "30px"
    container.style.marginLeft = "auto"
    container.style.marginRight = "auto"
    container.style.width = s"${W}px"
    container.style.height = s"${H}px"
    val d      = W / 4
    val canvas = document.createElement("canvas").asInstanceOf[Canvas]
    canvas.width = W
    canvas.height = H
    canvas.setAttribute("id", "canvas")
    canvas.style.width = s"${canvas.width}px"
    canvas.style.height = s"${canvas.height}px";
    val ratio = dom.window.devicePixelRatio
    canvas.width = (canvas.width * ratio).toInt
    canvas.height = (canvas.height * ratio).toInt
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.scale(ratio, ratio)
    val radius = d / 12
    for {
      i <- 0 to 3
      j <- 0 to 3
    } yield {
      drawRect(ctx, i * d, j * d, d - 4, d - 4, radius)
    }
    container.appendChild(canvas)
    container
  }
}
